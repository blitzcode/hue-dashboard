
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, LambdaCase, RecordWildCards #-}

module HueREST ( BridgeRequestMethod(..)
               , noBody
               , bridgeRequest
               , bridgeRequestTrace
               , bridgeRequestRetryTrace
               , BridgeError(..)
               , BridgeResponse(..)
               ) where

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Aeson
import Data.Monoid
import System.FilePath ((</>))
import Network.HTTP.Simple

import Util
import Trace

-- Interface to make calls to the REST / HTTP / JSON based API of a Hue bridge
--
-- http://www.developers.meethue.com/philips-hue-api

-- TODO: Add 'Offline' mode, returning captured bridge responses

data BridgeRequestMethod = MethodGET | MethodPOST | MethodPUT
                           deriving (Eq, Enum)

instance Show BridgeRequestMethod where
    show MethodGET  = "GET"
    show MethodPOST = "POST"
    show MethodPUT  = "PUT"

-- Makes it a little easier to do a request without a body
noBody :: Maybe Int
noBody = Nothing

-- Call a REST API on the Hue bridge
bridgeRequest :: forall m a body. (MonadIO m, MonadThrow m, FromJSON a, ToJSON body)
              => BridgeRequestMethod
              -> IPAddress
              -> Maybe body
              -> BridgeUserID
              -> String
              -> m a
bridgeRequest method bridgeIP mbBody userID apiEndPoint = do
    request' <- parseRequest $ show method <> " http://" <> fromIPAddress bridgeIP </> "api"
                               </> fromBridgeUserID userID </> apiEndPoint
    let request = case mbBody of
                      Just j  -> setRequestBodyJSON j request'
                      Nothing -> request'
    response <- httpJSON request
    return (getResponseBody response :: a)

-- TODO: Would be good to have an exception type to restart the entire server to recover
--       from a bridge IP change or something like that

-- Wrapper around bridgeRequest which traces errors and doesn't return anything, fire and forget
bridgeRequestTrace :: forall m body. ( MonadIO m
                                     , MonadThrow m
                                     , MonadCatch m
                                     , ToJSON body
                                     , Show body
                                     )
                        => BridgeRequestMethod
                        -> IPAddress
                        -> Maybe body
                        -> BridgeUserID
                        -> String
                        -> m ()
bridgeRequestTrace method bridgeIP mbBody userID apiEndPoint = do
    -- Trace requests where we change state /  request something
    when (method == MethodPUT || method == MethodPOST) $
        traceS TLInfo $ "bridgeRequestTrace: " <> show method <> " " <>
                        apiEndPoint <> " - " <>
                        case mbBody of Just b -> show b; _ -> ""
    r <- (Just <$> bridgeRequest method bridgeIP mbBody userID apiEndPoint) `catches`
           [ Handler $ \(e :: HttpException) -> do
               -- Network / IO error
               traceS TLError $ "bridgeRequestTrace: HTTP exception while contacting '"
                                <> apiEndPoint <> "' : " <> show e
               return Nothing
           , Handler $ \(e :: JSONException) -> do
               -- Parsing error
               traceS TLError $ "bridgeRequestTrace: JSON exception while contacting '"
                                <> apiEndPoint <> "' : " <> show e
               return Nothing
           ]
    case r of
        Just (ResponseOK (_ :: Value)) ->
            -- Success
            return ()
        Just err@(ResponseError { .. }) -> do
            -- Got an error from the bridge
            traceS TLError $ "bridgeRequestTrace: Error response from '"
                             <> apiEndPoint <> "' : " <> show err
        Nothing -> return ()

-- Wrapper around bridgeRequest which traces errors and retries automatically
bridgeRequestRetryTrace :: forall m a body. ( MonadIO m
                                            , MonadThrow m
                                            , MonadCatch m
                                            , FromJSON a
                                            , Show a -- TODO: For the show instance of the
                                                     ---      response, we actually never
                                                     --       print the result...
                                            , ToJSON body
                                            , Show body
                                            )
                        => BridgeRequestMethod
                        -> IPAddress
                        -> Maybe body
                        -> BridgeUserID
                        -> String
                        -> m a
bridgeRequestRetryTrace method bridgeIP mbBody userID apiEndPoint = do
    -- Trace requests where we change state /  request something
    when (method == MethodPUT || method == MethodPOST) $
        traceS TLInfo $ "bridgeRequestRetryTrace: " <> show method <> " " <>
                        apiEndPoint <> " - " <>
                        case mbBody of Just b -> show b; _ -> ""
    -- TODO: It makes sense to retry if we have a connection error, but in case of
    --       something like a parsing error or an access denied type response, an
    --       endless retry loop might not do anything productive
    r <- (Just <$> bridgeRequest method bridgeIP mbBody userID apiEndPoint) `catches`
           [ Handler $ \(e :: HttpException) -> do
               -- Network / IO error
               traceS TLError $ "bridgeRequestRetryTrace: HTTP exception while contacting '"
                                <> apiEndPoint <> "' (retry in 5s): " <> show e
               return Nothing
           , Handler $ \(e :: JSONException) -> do
               -- Parsing error
               traceS TLError $ "bridgeRequestRetryTrace: JSON exception while contacting '"
                                <> apiEndPoint <> "' (retry in 5s): " <> show e
               return Nothing
           ]
    case r of
        Just (ResponseOK (val :: a)) ->
            -- Success
            return val
        Just err@(ResponseError { .. }) -> do
            -- Got an error from the bridge
            traceS TLError $ "bridgeRequestRetryTrace: Error response from '"
                             <> apiEndPoint <> "' (retry in 5s): " <> show err
            waitNSec 5
            retry
        Nothing -> do
            waitNSec 5
            retry
  where
    retry = bridgeRequestRetryTrace method bridgeIP mbBody userID apiEndPoint

-- We add custom constructors for the bridge errors we actually want to handle, default
-- all others to BEOther
--
-- http://www.developers.meethue.com/documentation/error-messages
--
data BridgeError = BEUnauthorizedUser
                 | BELinkButtonNotPressed
                 | BEOther !Int
                   deriving (Eq, Show)

instance Enum BridgeError where
    toEnum 1   = BEUnauthorizedUser
    toEnum 101 = BELinkButtonNotPressed
    toEnum err = BEOther err
    fromEnum BEUnauthorizedUser     = 1
    fromEnum BELinkButtonNotPressed = 101
    fromEnum (BEOther err)          = err

-- Generic response type from the bridge. Either we get an array containing an object with
-- the 'error' key and the type / address / description fields, or we get our wanted response

data BridgeResponse a = ResponseError { reType :: !BridgeError
                                      , reAddr :: !String
                                      , reDesc :: !String
                                      }
                      | ResponseOK a
                        deriving Show

instance FromJSON a => FromJSON (BridgeResponse a) where
    parseJSON j = let parseError = do [(Object o)] <- parseJSON j
                                      err <- o .: "error"
                                      ResponseError <$> (toEnum <$> err .: "type")
                                                    <*>             err .: "address"
                                                    <*>             err .: "description"
                      parseOK    = ResponseOK <$> parseJSON j
                   in parseError <|> parseOK

