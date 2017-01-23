
{-# LANGUAGE LambdaCase, ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}

module HueSetup ( discoverBridgeIP
                , createUser
                ) where

import Data.Aeson
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import Text.Printf
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Lens
import Network.HostName
import Network.HTTP.Simple

import Util
import Trace
import HueREST
import HueJSON
import HueBroker

-- Before we can call the Hue API we need to discover the bridge IP and create a
-- whitelisted user by pushlinking

-- Verify existing bridge IP and / or discover new one
discoverBridgeIP :: (MonadIO m, MonadCatch m) => Maybe IPAddress -> m IPAddress
discoverBridgeIP bridgeIP =
    -- Do we have a bridge IP to try?
    case bridgeIP of
        Just ip -> do
            -- Verify bridge IP by querying the bridge config. This is one of the few API
            -- requests we can make without having a whitelisted user, use it to verify
            -- that our IP points to a valid Hue bridge
            traceS TLInfo $ "Trying to verify bridge IP: " <> fromIPAddress ip
            (r :: Maybe BridgeConfigNoWhitelist) <-
              (Just <$> bridgeRequest MethodGET ip noBody (BridgeUserID "no-user") "config")
                `catches`
                  [ Handler $ \(e :: HttpException) -> do
                      -- Network / IO error
                      traceS TLError $ "discoverBridgeIP: HTTP exception, bad / stale bridge IP: "
                                       <> show e
                      return Nothing
                  , Handler $ \(e :: JSONException) -> do
                      -- Parsing error
                      traceS TLError $ "discoverBridgeIP: JSON exception during bridge verify: '"
                                       <> show e
                      return Nothing
                  ]
            case r of
                Nothing  -> discoverBridgeIP Nothing
                Just cfg -> do
                    traceS TLInfo $ "Success, basic bridge configuration:\n" <> (show cfg)
                    -- Obtained and traced basic configuration, check API version
                    case cfg ^. bcnwAPIVersion of
                        av@(APIVersion { .. })
                            | avMajor >= 1 && avMinor >= 12 ->
                                traceS TLInfo $ "Bridge API version OK: " <> show av
                            | otherwise                     ->
                                traceS TLWarn $
                                    "API version lower than expected (1.12.0), " <>
                                    "please update the bridge to avoid incompatibilities: " <>
                                    show av
                    return ip
        Nothing -> do
            -- No IP, run bridge discovery
            traceS TLInfo "Running bridge discovery using broker server..."
            r <- (Just <$> queryBrokerServer) `catches`
                [ Handler $ \(e :: HttpException) -> do
                    -- Network / IO error
                    traceS TLError $ "discoverBridgeIP: HTTP exception while contacting broker: "
                                     <> show e
                    return Nothing
                , Handler $ \(e :: JSONException) -> do
                    -- Parsing error
                    traceS TLError $ "discoverBridgeIP: JSON exception during broker query: '"
                                     <> show e
                    return Nothing
                ]
            case r of
                Nothing -> do
                    traceS TLError $ "Bridge discovery failed (retry in 5s)"
                    waitNSec 5
                    discoverBridgeIP Nothing
                Just bridges -> do
                    if null bridges
                        then do traceS TLError "No bridge found (retry in 5s)"
                                waitNSec 5
                                discoverBridgeIP Nothing
                        else do traceS TLInfo $
                                    printf "Found %i bridge(s), using first:\n%s"
                                           (length bridges) (show bridges)
                                -- TODO: Try all bridges till we find a working
                                --       one instead of always going for the first
                                discoverBridgeIP . Just . brInternalIPAddress . head $ bridges

data UserNameResponse = UserNameResponse !BridgeUserID
                        deriving Show

instance FromJSON UserNameResponse where
    parseJSON j = do [(Object o)] <- parseJSON j
                     o' <- o .: "success"
                     UserNameResponse <$> o' .: "username"

-- Verify existing user or create new one
createUser :: (MonadIO m, MonadCatch m) => IPAddress -> Maybe BridgeUserID -> m BridgeUserID
createUser bridgeIP userID =
    -- Do we have a user ID to try?
    case userID of
        Just uid -> do
            -- Verify user ID by querying timezone list, which requires whitelisting
            traceS TLInfo $ "Trying to verify user ID: " <> fromBridgeUserID uid
            r <- (Just <$> bridgeRequest MethodGET bridgeIP noBody uid "info/timezones") `catches`
                [ Handler $ \(e :: HttpException) -> do
                    -- Network / IO error
                    traceS TLError $ "createUser: HTTP exception during user verify: "
                                     <> show e
                    return Nothing
                , Handler $ \(e :: JSONException) -> do
                    -- Parsing error
                    traceS TLError $ "createUser: JSON exception during user verify: '"
                                     <> show e
                    return Nothing
                ]
            case r of
                Nothing  ->  do
                    traceS TLError $ "Failed to verify user ID (retry in 5s)"
                    waitNSec 5
                    createUser bridgeIP (Just uid)
                Just err@(ResponseError { .. }) -> do
                    -- Got an error from the bridge, just create a fresh user ID
                    traceS TLError $ "Error response verifying user ID: " <> show err
                    createUser bridgeIP Nothing
                Just (ResponseOK (_ :: [String])) -> do
                    -- Looks like we got our timezone list, user ID is whitelisted and verified
                    traceS TLInfo $ "User ID seems whitelisted"
                    return uid
        Nothing  -> do
            -- Create new user on the bridge
            --
            -- http://www.developers.meethue.com/documentation/configuration-api#71_create_user
            --
            host <- liftIO getHostName
            -- We use our application name and the host name, as recommended. Truncate the host
            -- name to the API limit
            let body = HM.fromList ( [("devicetype", "hue-dashboard#" <> take 19 host)]
                                     :: [(String, String)]
                                   )
            traceS TLInfo $ "Creating new user ID: " <> show body
            r <- (Just <$> bridgeRequest MethodPOST bridgeIP (Just body) (BridgeUserID "") "")
              `catches`
                [ Handler $ \(e :: HttpException) -> do
                    -- Network / IO error
                    traceS TLError $ "createUser: HTTP exception during user user create: "
                                     <> show e
                    return Nothing
                , Handler $ \(e :: JSONException) -> do
                    -- Parsing error
                    traceS TLError $ "createUser: JSON exception during user user create: '"
                                     <> show e
                    return Nothing
                ]
            case r of
                Nothing -> do
                    -- Network / IO / parsing error, retry
                    traceS TLError $ "Failed to create user ID (retry in 5s)"
                    waitNSec 5
                    createUser bridgeIP Nothing
                Just err@(ResponseError { .. }) -> do
                    -- Error from the bridge, try again and alert user if we require the
                    -- link button on the bridge to be pressed
                    traceS TLError $
                        "Error response creating user ID (retry in 5s): " <> show err
                    case reType of
                        BELinkButtonNotPressed ->
                            traceS TLWarn $
                                "Please press the link button on the bridge to proceed"
                        _ -> return ()
                    waitNSec 5
                    createUser bridgeIP Nothing
                Just (ResponseOK (UserNameResponse uid)) -> do
                    -- We created the user, recurse to verify whitelisting
                    traceS TLInfo $ "Successfully created new user ID: " <> fromBridgeUserID uid
                    createUser bridgeIP (Just uid)

