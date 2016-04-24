
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, LambdaCase, RecordWildCards #-}

module HueREST ( BridgeRequestMethod(..)
               , noBody
               , bridgeRequest
               , bridgeRequestTrace
               , bridgeRequestRetryTrace
               , BridgeError(..)
               , BridgeResponse(..)
               ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Aeson
import Data.Monoid
import qualified Data.ByteString.Char8 as B8
import System.FilePath ((</>))
import Network.HTTP.Client (isIpAddress)
import Network.HTTP.Simple

import Util
import Trace

-- Interface to make calls to the REST / HTTP / JSON based API of a Hue bridge
--
-- http://www.developers.meethue.com/philips-hue-api

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
              -> String
              -> String
              -> m a
bridgeRequest method bridgeIP mbBody userID apiEndPoint = do
    unless (isIpAddress $ B8.pack bridgeIP) $ fail "Invalid IP address"
    request' <- parseRequest $
                    show method <> " http://" <> bridgeIP </> "api" </> userID </> apiEndPoint
    let request = case mbBody of
                      Just j  -> setRequestBodyJSON j request'
                      Nothing -> request'
    response <- httpJSON request
    return (getResponseBody response :: a)

-- TODO: bridgeRequestTrace and bridgeRequestRetryTrace currently catch all exceptions,
--       including things like a CTRL+C 'user interrupt'. This is poor practice, write
--       specific handlers for the HTTP and JSON exceptions we care about

-- Wrapper around bridgeRequest which traces errors and doesn't return anything, fire and forget
bridgeRequestTrace :: forall m body. ( MonadIO m
                                     , MonadThrow m
                                     , MonadCatch m
                                     , ToJSON body
                                     )
                        => BridgeRequestMethod
                        -> IPAddress
                        -> Maybe body
                        -> String
                        -> String
                        -> m ()
bridgeRequestTrace method bridgeIP mbBody userID apiEndPoint = do
    try (bridgeRequest method bridgeIP mbBody userID apiEndPoint) >>= \case
        Left (e :: SomeException) -> do
            -- Network / IO / parsing error
            traceS TLError $ "bridgeRequestTrace: Exception while contacting / processing '"
                             <> apiEndPoint <> "' : " <> show e
        Right err@(ResponseError { .. }) -> do
            -- Got an error from the bridge
            traceS TLError $ "bridgeRequestTrace: Error response from '"
                             <> apiEndPoint <> "' : " <> show err
        Right (ResponseOK (_ :: Value)) ->
            -- Success
            return ()

-- Wrapper around bridgeRequest which traces errors and retries automatically
bridgeRequestRetryTrace :: forall m a body. ( MonadIO m
                                            , MonadThrow m
                                            , MonadCatch m
                                            , FromJSON a
                                            , Show a -- TODO: For the show instance of the
                                                     ---      response, we actually never
                                                     --       print the result...
                                            , ToJSON body
                                            )
                        => BridgeRequestMethod
                        -> IPAddress
                        -> Maybe body
                        -> String
                        -> String
                        -> m a
bridgeRequestRetryTrace method bridgeIP mbBody userID apiEndPoint = do
    try (bridgeRequest method bridgeIP mbBody userID apiEndPoint) >>= \case
        -- TODO: It makes sense to retry if we have a connection error, but in case of
        --       something like a parsing error or an access denied type response, an
        --       endless retry loop might not do anything productive
        Left (e :: SomeException) -> do
            -- Network / IO / parsing error
            traceS TLError $ "bridgeRequestRetryTrace: Exception while contacting / processing '"
                             <> apiEndPoint <> "' (retry in 5s): " <> show e
            waitNSec 5
            retry
        Right err@(ResponseError { .. }) -> do
            -- Got an error from the bridge
            traceS TLError $ "bridgeRequestRetryTrace: Error response from '"
                             <> apiEndPoint <> "' (retry in 5s): " <> show err
            waitNSec 5
            retry
        Right (ResponseOK (val :: a)) -> do
            -- Success
            return val
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

