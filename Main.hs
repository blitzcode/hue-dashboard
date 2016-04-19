
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Network.HTTP.Client (isIpAddress)
import Network.HTTP.Simple
import System.Exit
import Data.Aeson
import Control.Monad
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
discoverBridges :: IO [Bridge]
discoverBridges = do
    let brokerServerURL = "https://www.meethue.com/api/nupnp"
    request  <- parseRequest brokerServerURL
    response <- httpJSON request
    return (getResponseBody response :: [Bridge])

main :: IO ()
main = withTrace Nothing True False True TLInfo $ do
    -- Bridge discovery (TODO: Don't run this on every startup)
    traceS TLInfo "Running bridge discovery..."
    bridges <- discoverBridges
    when (null bridges) $
        traceS TLError "No bridge found" >> exitFailure
    traceS TLInfo $ printf "Found %i bridge(s):\n%s" (length bridges) (show bridges)

