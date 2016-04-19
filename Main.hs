
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Network.HTTP.Client (isIpAddress)
import Network.HTTP.Simple
import System.Exit
import Data.Aeson
import Data.Monoid
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Catch
import qualified Data.ByteString.Char8 as B8
import Text.Printf

import Trace

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

-- Find the IP of the Hue bridge by using the broker server
-- http://www.developers.meethue.com/documentation/hue-bridge-discovery
discoverBridges :: (MonadThrow m, MonadIO m) => m [Bridge]
discoverBridges = do
    let brokerServerURL = "https://www.meethue.com/api/nupnp"
    request  <- parseRequest brokerServerURL
    response <- httpJSON request
    return (getResponseBody response :: [Bridge])

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
getBridgeConfig :: (MonadIO m, MonadThrow m) => Bridge -> m BridgeConfig
getBridgeConfig bridge = do
    request  <- parseRequest $ "GET http://" <> brInternalIPAddress bridge <> "/api/no-user/config"
    response <- httpJSON request
    return (getResponseBody response :: BridgeConfig)

-- Configuration data which we persist in a configuration file
data PersistConfig = PersistConfig
    { pcBridgeIP :: String
    , pcUserID   :: String
    }

data AppState = AppState
    {
        asPC :: PersistConfig
    }

main :: IO ()
main =
  -- Setup tracing and our application state monad
  withTrace Nothing True False True TLInfo $ do
    flip evalStateT AppState { } $ do
      -- Bridge discovery
      traceS TLInfo "Running bridge discovery using broker server..."
      bridges <- discoverBridges
      when (null bridges) $
          traceS TLError "No bridge found" >> liftIO exitFailure
      traceS TLInfo $ printf "Found %i bridge(s), using first:\n%s" (length bridges) (show bridges)
      traceS TLInfo "Getting bridge configuration..."
      bridgeCfg <- getBridgeConfig $ head bridges
      traceS TLInfo $ show bridgeCfg

