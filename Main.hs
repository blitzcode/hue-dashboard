
{-# LANGUAGE   OverloadedStrings
             , RecordWildCards
             , DeriveGeneric
             , TemplateHaskell
             , LambdaCase
             , ScopedTypeVariables #-}

module Main where

import Network.HTTP.Client (isIpAddress)
import Network.HTTP.Simple
import System.FilePath
import Data.Aeson hiding ((.=))
import Data.Monoid
import Data.Maybe
import qualified Data.Yaml as Y
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Catch
import Control.Concurrent
import qualified Data.ByteString.Char8 as B8
import Text.Printf
import GHC.Generics

import Trace

type IPAddress = String

-- Bridge description obtained from the broker server
data Bridge = Bridge
    { brID                :: String
    , brInternalIPAddress :: IPAddress
    , brName              :: Maybe String
    , brMacAddress        :: Maybe String
    }

instance FromJSON Bridge where
    parseJSON (Object o) = do
        ip <- o .:  "internalipaddress"
        unless (isIpAddress $ B8.pack ip) $ fail "Invalid IP address"
        Bridge <$> o .:  "id"
               <*> pure ip
               <*> o .:? "name"
               <*> o .:? "macaddress"
    parseJSON _ = fail "Expected object"

instance Show Bridge where
    show Bridge { .. } = printf "Name: %s, ID: %s, IP: %s, Mac: %s"
                               (maybe "(None)" show brName)
                               (show brID)
                               (show brInternalIPAddress)
                               (maybe "(None)" show brMacAddress)

-- Discover local Hue bridges using the broker server
-- http://www.developers.meethue.com/documentation/hue-bridge-discovery
queryBrokerServer :: (MonadThrow m, MonadIO m) => m [Bridge]
queryBrokerServer = do
    let brokerServerURL = "https://www.meethue.com/api/nupnp"
    request  <- parseRequest brokerServerURL
    response <- httpJSON request
    return (getResponseBody response :: [Bridge])

-- Bridge configuration obtained from the api/config endpoint without a whitelisted user
data BridgeConfigNoWhitelist = BridgeConfigNoWhitelist
    { bcnwSWVersion  :: String
    , bcnwAPIVersion :: String
    , bcnwName       :: String
    , bcnwMac        :: String
    }

instance Show BridgeConfigNoWhitelist where
    show BridgeConfigNoWhitelist { .. } = printf "Name: %s, SW Ver: %s, API Ver: %s, Mac: %s"
                                                 (show bcnwName)
                                                 (show bcnwSWVersion)
                                                 (show bcnwAPIVersion)
                                                 (show bcnwMac)

instance FromJSON BridgeConfigNoWhitelist where
    parseJSON (Object o) = do
        BridgeConfigNoWhitelist <$> o .: "swversion"
                                <*> o .: "apiversion"
                                <*> o .: "name"
                                <*> o .: "mac"
    parseJSON _ = fail "Expected object"

-- Call a REST API on the Hue bridge
bridgeRequest :: forall m a. (MonadIO m, MonadThrow m, FromJSON a)
              => IPAddress
              -> String
              -> String
              -> m a
bridgeRequest bridgeIP userID apiEndPoint = do
    unless (isIpAddress $ B8.pack bridgeIP) $ fail "Invalid IP address"
    request  <- parseRequest $ "GET http://" <> bridgeIP </> "api" </> userID </> apiEndPoint
    response <- httpJSON request
    return (getResponseBody response :: a)

-- Configuration data which we persist in a file
data PersistConfig = PersistConfig
    { _pcBridgeIP :: IPAddress
    , _pcUserID   :: String
    } deriving (Generic, Show)

makeLenses ''PersistConfig

instance FromJSON PersistConfig

-- Application state
data AppState = AppState
    { _asPC        :: PersistConfig
    }

makeLenses ''AppState

-- Our main application monad
type AppT m = StateT AppState m
type AppIO = AppT IO

-- Load / store / create persistent configuration
defaultPersistConfig :: PersistConfig
defaultPersistConfig = PersistConfig "" ""

loadConfig :: MonadIO m => FilePath -> m (Maybe PersistConfig)
loadConfig fn = do
    -- Try to load persistent configuration into the state
    traceS TLInfo "Loading persistent configuration..."
    (liftIO $ Y.decodeFileEither fn) >>= \case
        Left e    -> do traceS TLError $
                         "Can't load configuration: " <> (Y.prettyPrintParseException e)
                        return Nothing
        Right cfg -> do traceS TLInfo $ "Configuration: %s" <> show cfg
                        return $ Just cfg

storeConfig :: MonadIO m => FilePath -> PersistConfig -> m ()
storeConfig fn cfg = do
    return () -- TODO

waitNSec :: MonadIO m => Int -> m ()
waitNSec sec = liftIO . threadDelay $ sec * 1000 * 1000

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
            try (bridgeRequest ip "no-user" "config") >>= \case
                Left (e :: SomeException) -> do
                    traceS TLError $ "Bad bridge IP: " <> (show e)
                    discoverBridgeIP Nothing
                Right (cfg :: BridgeConfigNoWhitelist) -> do
                    traceS TLInfo $ "Success, bridge configuration: "
                           <> (show cfg)
                    return ip
        Nothing -> do
            -- No IP, run bridge discovery
            traceS TLInfo "Running bridge discovery using broker server..."
            try queryBrokerServer >>= \case
                Left (e :: SomeException) -> do
                    traceS TLError $ "Bridge discovery failed (retry in 5s): " <> (show e)
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

-- Verify existing user or create new one
setupUser :: MonadIO m => IPAddress -> Maybe String -> m String
setupUser bridgeIP userID = {-
    -- Do we have a user ID to try?
    case userID of
        Just uid -> do
            -- Verify user ID by querying timezone list
            traceS TLInfo $ "Trying to verify user ID: " <> uid
            try (bridgeRequest bridgeIP uid "info/timezones") >>= \case
                Left (e :: SomeException) -> do
                    traceS TLError $ "Error verifying user ID: " <> (show e)
                    discoverBridgeIP Nothing
                Right (cfg :: BridgeConfigNoWhitelist) -> do
                    traceS TLInfo $ "Success, bridge configuration: "
                           <> (show cfg)
                    return ip
                    setupUser bridgeIP
        Nothing     -> do
            setupUser-}
    return ""

main :: IO ()
main =
  -- Setup tracing
  withTrace Nothing True False True TLInfo $ do
    -- Load configuration (might not be there)
    let configFile = "./config.yaml"
    mbCfg <- loadConfig configFile
    -- Bridge connection and user ID
    bridgeIP <- discoverBridgeIP   $ view pcBridgeIP <$> mbCfg
    userID   <- setupUser bridgeIP $ view pcUserID   <$> mbCfg
    -- We have everything setup, build and store configuration
    let newCfg = (fromMaybe defaultPersistConfig mbCfg)
                     & pcBridgeIP .~ bridgeIP
                     & pcUserID   .~ userID
    storeConfig configFile newCfg
    -- Setup application monad
    flip evalStateT AppState { _asPC = newCfg
                             } $ do
      return ()

