
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
            traceS TLInfo $ "Trying to verify bridge IP: " <> ip
            try (bridgeRequest MethodGET ip noBody "no-user" "config") >>= \case
                Left (e :: SomeException) -> do
                    traceS TLError $ "Bad / stale bridge IP: " <> show e
                    discoverBridgeIP Nothing
                Right (cfg :: BridgeConfigNoWhitelist) -> do
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
            try queryBrokerServer >>= \case
                Left (e :: SomeException) -> do
                    traceS TLError $ "Bridge discovery failed (retry in 5s): " <> show e
                    waitNSec 5
                    discoverBridgeIP Nothing
                Right bridges ->
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

data UserNameResponse = UserNameResponse !String
                        deriving Show

instance FromJSON UserNameResponse where
    parseJSON j = do [(Object o)] <- parseJSON j
                     o' <- o .: "success"
                     UserNameResponse <$> o' .: "username"

-- Verify existing user or create new one
createUser :: (MonadIO m, MonadCatch m) => IPAddress -> Maybe String -> m String
createUser bridgeIP userID =
    -- Do we have a user ID to try?
    case userID of
        Just uid -> do
            -- Verify user ID by querying timezone list, which requires whitelisting
            traceS TLInfo $ "Trying to verify user ID: " <> uid
            try (bridgeRequest MethodGET bridgeIP noBody uid "info/timezones") >>= \case
                Left (e :: SomeException) -> do
                    -- Network / IO / parsing error
                    traceS TLError $ "Failed to verify user ID (retry in 5s): " <> show e
                    waitNSec 5
                    createUser bridgeIP (Just uid)
                Right err@(ResponseError { .. }) -> do
                    -- Got an error from the bridge, just create a fresh user ID
                    traceS TLError $
                        "Error response verifying user ID: " <> show err
                    createUser bridgeIP Nothing
                Right (ResponseOK (_ :: [String])) -> do
                    -- Looks like we got our timezone list, user ID is whitelisted and verified
                    traceS TLInfo $ "User ID seems whitelisted"
                    return uid
        Nothing  -> do
            -- Create new user on the bridge
            --
            -- http://www.developers.meethue.com/documentation/configuration-api#71_create_user
            --
            host <- liftIO getHostName
            let body = -- We use our application name and the host name, as recommended
                       HM.fromList ([("devicetype", "hue-dashboard#" <> host)] :: [(String, String)])
            traceS TLInfo $ "Creating new user ID: " <> show body
            try (bridgeRequest MethodPOST bridgeIP (Just body) "" "") >>= \case
                Left (e :: SomeException) -> do
                    -- Network / IO / parsing error, retry
                    traceS TLError $ "Failed to create user ID (retry in 5s): " <> show e
                    waitNSec 5
                    createUser bridgeIP Nothing
                Right err@(ResponseError { .. }) -> do
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
                Right (ResponseOK (UserNameResponse uid)) -> do
                    -- We created the user, recurse to verify whitelisting
                    traceS TLInfo $ "Successfully created new user ID: " <> uid
                    createUser bridgeIP (Just uid)

