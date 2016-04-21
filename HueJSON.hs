
{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module HueJSON where

import Data.Aeson
import Data.Monoid
import Data.Attoparsec.Text
import Control.Lens

import Util

-- Records, lenses and JSON instances for communication with a Hue bridge

data APIVersion = APIVersion { avMajor :: !Int, avMinor :: !Int, avPatch :: !Int }

-- Bridge configuration obtained from the api/config endpoint without a whitelisted user
data BridgeConfigNoWhitelist = BridgeConfigNoWhitelist
    { _bcnwSWVersion  :: !String
    , _bcnwAPIVersion :: !APIVersion
    , _bcnwName       :: !String
    , _bcnwMac        :: !String
    } deriving Show

-- Actual bridge configuration obtainable by whitelisted user. We only parse a selection
-- of potentially interesting fields
--
-- http://www.developers.meethue.com/documentation/configuration-api#72_get_configuration
--
data BridgeConfig = BridgeConfig
    { _bcName             :: !String
    , _bcZigBeeChannel    :: !Int
    , _bcBridgeID         :: !String
    , _bcMac              :: !String
    , _bcIPAddress        :: !IPAddress
    , _bcNetmask          :: !String
    , _bcGateway          :: !String
    , _bcModelID          :: !String
    , _bcSWVersion        :: !String
    , _bcAPIVersion       :: !APIVersion
    , _bcSWUpdate         :: !(Maybe SWUpdate)
    , _bcLinkButton       :: !Bool
    , _bcPortalServices   :: !Bool
    , _bcPortalConnection :: !String
    , _bcPortalState      :: !(Maybe PortalState)
    , _bcFactoryNew       :: !Bool
    } deriving Show

data SWUpdate = SWUpdate
    { _swuUpdateState    :: !Int
    , _swuCheckForUpdate :: !Bool
    , _swuURL            :: !String
    , _swuText           :: !String
    , _swuNotify         :: !Bool
    } deriving Show

data PortalState = PortalState
    { _psSignedOn      :: !Bool
    , _psIncoming      :: !Bool
    , _psOutgoing      :: !Bool
    , _psCommunication :: !String
    } deriving Show

instance FromJSON BridgeConfigNoWhitelist where
    parseJSON (Object o) =
        BridgeConfigNoWhitelist <$> o .: "swversion"
                                <*> o .: "apiversion"
                                <*> o .: "name"
                                <*> o .: "mac"
    parseJSON _ = fail "Expected object"

instance FromJSON BridgeConfig where
    parseJSON (Object o) =
        BridgeConfig <$> o .:  "name"
                     <*> o .:  "zigbeechannel"
                     <*> o .:  "bridgeid"
                     <*> o .:  "mac"
                     <*> o .:  "ipaddress"
                     <*> o .:  "netmask"
                     <*> o .:  "gateway"
                     <*> o .:  "modelid"
                     <*> o .:  "swversion"
                     <*> o .:  "apiversion"
                     <*> o .:? "swupdate"
                     <*> o .:  "linkbutton"
                     <*> o .:  "portalservices"
                     <*> o .:  "portalconnection"
                     <*> o .:? "portalstate"
                     <*> o .:  "factorynew"
    parseJSON _ = fail "Expected object"

instance FromJSON SWUpdate where
    parseJSON (Object o) =
        SWUpdate <$> o .: "updatestate"
                 <*> o .: "checkforupdate"
                 <*> o .: "url"
                 <*> o .: "text"
                 <*> o .: "notify"
    parseJSON _ = fail "Expected object"

instance FromJSON PortalState where
    parseJSON (Object o) =
        PortalState <$> o .: "signedon"
                    <*> o .: "incoming"
                    <*> o .: "outgoing"
                    <*> o .: "communication"
    parseJSON _ = fail "Expected object"

instance FromJSON APIVersion where
    parseJSON (String s) =
        either (fail "Failed to parse version number")
               return
               (parseOnly parser s)
      where parser = APIVersion <$> (decimal <* char '.') <*> (decimal <* char '.') <*> decimal
    parseJSON _ = fail "Expected string"

instance Show APIVersion where
    show APIVersion { .. } = show avMajor <> "." <> show avMinor <> "." <> show avPatch

makeLenses ''BridgeConfigNoWhitelist
makeLenses ''BridgeConfig
makeLenses ''SWUpdate
makeLenses ''PortalState

