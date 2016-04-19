
{-# LANGUAGE   OverloadedStrings
             , RecordWildCards
             , DeriveGeneric
             , TemplateHaskell
             , LambdaCase
             , ScopedTypeVariables #-}

module Main where

import Network.HTTP.Client (isIpAddress)
import Network.HTTP.Simple
import System.FilePath ()
import Data.Aeson hiding ((.=))
import Data.Monoid
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

-- Bridge description obtained from the broker server
data Bridge = Bridge
    { brID                :: String
    , brInternalIPAddress :: String
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
discoverBridges :: (MonadThrow m, MonadIO m) => m [Bridge]
discoverBridges = do
    let brokerServerURL = "https://www.meethue.com/api/nupnp"
    request  <- parseRequest brokerServerURL
    response <- httpJSON request
    return (getResponseBody response :: [Bridge])

-- Bridge configuration obtained from the api/config endpoint
data BridgeConfig = BridgeConfig
    { bcSWVersion  :: String
    , bcAPIVersion :: String
    , bcName       :: String
    , bcMac        :: String
    }

instance Show BridgeConfig where
    show BridgeConfig { .. } = printf "Name: %s, SW Ver: %s, API Ver: %s, Mac: %s"
                                   (show bcName)
                                   (show bcSWVersion)
                                   (show bcAPIVersion)
                                   (show bcMac)

instance FromJSON BridgeConfig where
    parseJSON (Object o) = do
        BridgeConfig <$> o .: "swversion"
                     <*> o .: "apiversion"
                     <*> o .: "name"
                     <*> o .: "mac"
    parseJSON _ = fail "Expected object"

-- Get the basic bridge configuration. This is one of the few API requests we can make
-- without having a linked user. We use this to check if the IP we have is actually
-- pointing to a working Hue bridge
getBridgeConfig :: (MonadIO m, MonadThrow m) => String -> m BridgeConfig
getBridgeConfig bridgeIP = do
    unless (isIpAddress $ B8.pack bridgeIP) $ fail "Invalid IP address"
    request  <- parseRequest $ "GET http://" <> bridgeIP <> "/api/no-user/config"
    response <- httpJSON request
    return (getResponseBody response :: BridgeConfig)

-- Configuration data which we persist in a configuration file
data PersistConfig = PersistConfig
    { _pcBridgeIP :: Maybe String
    , _pcUserID   :: Maybe String
    } deriving (Generic, Show)

makeLenses ''PersistConfig

instance FromJSON PersistConfig

-- Application state
data AppState = AppState
    { _asPC        :: PersistConfig
    , _asBridgeCfg :: Maybe BridgeConfig
    }

makeLenses ''AppState

-- Our main application monad
type AppT m = StateT AppState m
type AppIO = AppT IO

-- Load / store / create persistent configuration
defaultPersistConfig :: PersistConfig
defaultPersistConfig = PersistConfig Nothing Nothing

configFile :: FilePath
configFile = "./config.yaml"

tryLoadConfig :: AppIO ()
tryLoadConfig = do
    -- Try to load persistent configuration into the state
    traceS TLInfo "Loading persistent configuration..."
    liftIO (Y.decodeFileEither configFile) >>= \case
        Left e    -> traceS TLError $
                         "Can't load configuration: " <> (Y.prettyPrintParseException e)
        Right cfg -> do asPC .= cfg
                        traceS TLInfo $ "Configuration: %s" <> show cfg

waitNSec :: MonadIO m => Int -> m ()
waitNSec sec = liftIO . threadDelay $ sec * 1000 * 1000

main :: IO ()
main =
  -- Setup tracing and our application monad
  withTrace Nothing True False True TLInfo $
  flip evalStateT AppState { _asPC        = defaultPersistConfig
                           , _asBridgeCfg = Nothing
                           } $ do
    -- Load configuration data from last run
    tryLoadConfig
    -- Discover bridge IP
    let bridgeDiscoverLoop = do
            -- Do we have a bridge IP to try?
            use (asPC . pcBridgeIP) >>= \case
                Just ip -> do
                    -- Verify bridge IP by querying bridge config
                    traceS TLInfo $ "Trying to verify bridge IP: " <> ip
                    try (getBridgeConfig ip) >>= \case
                        Left (e :: SomeException) -> do
                            traceS TLError $ "Bad bridge IP: " <> (show e)
                            asPC . pcBridgeIP .= Nothing
                            bridgeDiscoverLoop
                        Right cfg -> do
                            traceS TLInfo $ "Success, bridge configuration: "
                                   <> (show cfg)
                            asBridgeCfg .= Just cfg
                Nothing -> do
                    -- No IP, run bridge discovery
                    traceS TLInfo "Running bridge discovery using broker server..."
                    try discoverBridges >>= \case
                        Left (e :: SomeException) -> do
                            traceS TLError $ "Bridge discovery failed (retry in 5s): " <> (show e)
                            waitNSec 5
                        Right bridges ->
                            if null bridges
                                then traceS TLError "No bridge found (retry in 5s)" >> waitNSec 5
                                else do traceS TLInfo $
                                            printf "Found %i bridge(s), using first:\n%s"
                                                   (length bridges) (show bridges)
                                        -- TODO: Try all bridges till we find a working
                                        --       one instead of always going for the first
                                        asPC . pcBridgeIP .=
                                            (Just . brInternalIPAddress . head $ bridges)
                    bridgeDiscoverLoop
     in bridgeDiscoverLoop
    -- Create user

    -- We have a working bridge connection and user, write configuration data for next time

