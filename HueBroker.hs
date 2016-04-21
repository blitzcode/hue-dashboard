
{-# LANGUAGE OverloadedStrings #-}

module HueBroker ( BrokerBridge(..)
                 , queryBrokerServer
                 ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Network.HTTP.Client (isIpAddress)
import Network.HTTP.Simple
import Data.Aeson
import qualified Data.ByteString.Char8 as B8

import Util

-- Discover local Hue bridges using the broker server
--
-- http://www.developers.meethue.com/documentation/hue-bridge-discovery

-- Bridge description obtained from the broker server
data BrokerBridge = BrokerBridge
    { brID                :: !String
    , brInternalIPAddress :: !IPAddress
    , brName              :: !(Maybe String)
    , brMacAddress        :: !(Maybe String)
    } deriving Show

instance FromJSON BrokerBridge where
    parseJSON (Object o) = do
        ip <- o .:  "internalipaddress"
        unless (isIpAddress $ B8.pack ip) $ fail "Invalid IP address"
        BrokerBridge <$> o .:  "id"
                     <*> pure ip
                     <*> o .:? "name"
                     <*> o .:? "macaddress"
    parseJSON _ = fail "Expected object"

queryBrokerServer :: (MonadThrow m, MonadIO m) => m [BrokerBridge]
queryBrokerServer = do
    let brokerServerURL = "https://www.meethue.com/api/nupnp"
    request  <- parseRequest brokerServerURL
    response <- httpJSON request
    return (getResponseBody response :: [BrokerBridge])

