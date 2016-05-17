
{-# LANGUAGE   TemplateHaskell
             , OverloadedStrings
             , RecordWildCards
             , LambdaCase
             , ScopedTypeVariables
             , FlexibleInstances
             , TypeSynonymInstances
             , GeneralizedNewtypeDeriving #-}

module HueJSON where

import Data.Aeson
import Data.Monoid
import Data.Char
import Data.Word
import Data.Time
import Data.Attoparsec.Text
import Data.Hashable
import Data.Coerce
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Control.Lens

import Util

-- Records, lenses and JSON instances for communication with a Hue bridge

-- TODO: Add representations for sensor data

data Light = Light { _lgtState             :: !LightState
                   , _lgtType              :: !ZLL_LightType
                   , _lgtName              :: !String
                   , _lgtModelID           :: !LightModel
                   , _lgtManufacturerName  :: !String
                   , _lgtLuminaireUniqueID :: !(Maybe String)
                   , _lgtUniqueID          :: !String
                   , _lgtSWVersion         :: !String
                   } deriving Show

instance FromJSON Light where
    parseJSON (Object o) = Light <$> o .:  "state"
                                 <*> o .:  "type"
                                 <*> o .:  "name"
                                 <*> o .:  "modelid"
                                 <*> o .:  "manufacturername"
                                 <*> o .:? "luminaireuniqueid"
                                 <*> o .:  "uniqueid"
                                 <*> o .:  "swversion"
    parseJSON _ = fail "Expected object"

-- TODO: Convert the various string arguments to proper ADTs
data LightState = LightState { _lsOn         :: !Bool
                             , _lsBrightness :: !(Maybe Word8)
                             , _lsHue        :: !(Maybe Word16)
                             , _lsSaturation :: !(Maybe Word8)
                             , _lsEffect     :: !(Maybe String)
                             , _lsXY         :: !(Maybe [Float])
                             , _lsColorTemp  :: !(Maybe Word16)
                             , _lsAlert      :: !String
                             , _lsColorMode  :: !(Maybe ColorMode)
                             , _lsReachable  :: !Bool
                             } deriving Show

instance FromJSON LightState where
    parseJSON (Object o) = LightState <$> o .:  "on"
                                      <*> o .:? "bri"
                                      <*> o .:? "hue"
                                      <*> o .:? "sat"
                                      <*> o .:? "effect"
                                      <*> o .:? "xy"
                                      <*> o .:? "ct"
                                      <*> o .:  "alert"
                                      <*> o .:? "colormode"
                                      <*> o .:  "reachable"
    parseJSON _ = fail "Expected object"

data ColorMode = CMXY | CMCT | CMHS
                 deriving (Eq, Show, Enum)

instance FromJSON ColorMode where
    parseJSON (String s) = case T.unpack s of
                               "xy" -> return CMXY
                               "ct" -> return CMCT
                               "hs" -> return CMHS
                               str  -> fail $ "Invalid color mode: " <> str
    parseJSON _ = fail "Expected string"

-- Light type
--
-- http://www.developers.meethue.com/documentation/supported-lights
-- http://cache.nxp.com/documents/user_manual/JN-UG-3091.pdf
data ZLL_LightType = LT_OnOffLight
                   | LT_OnOffPlugInUnit
                   | LT_DimmableLight
                   | LT_DimmablePlugInUnit
                   | LT_ColorLight
                   | LT_ExtendedColorLight
                   | LT_ColorTemperatureLight
                     deriving Enum

instance FromJSON ZLL_LightType where
    parseJSON (String s) =
        case map toLower . T.unpack $ s of
            "on/off light"            -> return LT_OnOffLight
            "on/off plug-in unit"     -> return LT_OnOffPlugInUnit
            "dimmable light"          -> return LT_DimmableLight
            "dimmable plug-in unit"   -> return LT_DimmablePlugInUnit
            "color light"             -> return LT_ColorLight
            "extended color light"    -> return LT_ExtendedColorLight
            "color temperature light" -> return LT_ColorTemperatureLight
            str                       -> fail $ "Unknown / invalid ZLL light type: " <> str
    parseJSON _ = fail "Expected string"

instance Show ZLL_LightType where
    show LT_OnOffLight            = "On/Off Light"
    show LT_OnOffPlugInUnit       = "On/Off Plug-in Unit"
    show LT_DimmableLight         = "Dimmable Light"
    show LT_DimmablePlugInUnit    = "Dimmable Plug-in Unit"
    show LT_ColorLight            = "Color Light"
    show LT_ExtendedColorLight    = "Extended Color Light"
    show LT_ColorTemperatureLight = "Color Temperature Light"

isColorLT :: ZLL_LightType -> Bool
isColorLT = \case LT_ColorLight         -> True
                  LT_ExtendedColorLight -> True
                  _                     -> False

isCTOnlyLight :: ZLL_LightType -> Bool
isCTOnlyLight = \case LT_ColorTemperatureLight -> True
                      _                        -> False

isCTLight :: ZLL_LightType -> Bool
isCTLight = \case LT_ColorTemperatureLight -> True
                  LT_ExtendedColorLight    -> True
                  _                        -> False

isDimmableLT :: ZLL_LightType -> Bool
isDimmableLT = \case LT_ColorLight            -> True
                     LT_ExtendedColorLight    -> True
                     LT_ColorTemperatureLight -> True
                     LT_DimmableLight         -> True
                     LT_DimmablePlugInUnit    -> True
                     _                        -> False

-- Light model
--
-- http://www.developers.meethue.com/documentation/supported-lights

data LightModel = LM_HueBulbA19
                | LM_HueSpotBR30
                | LM_HueSpotGU10
                | LM_HueLightStrips
                | LM_HueLivingColorsIris
                | LM_HueLivingColorsBloom
                | LM_LivingColorsGen3Iris
                | LM_LivingColorsGen3BloomAura
                | LM_HueA19Lux
                | LM_ColorLightModule
                | LM_ColorTemperatureModule
                | LM_HueA19WhiteAmbience
                | LM_HueGo
                | LM_HueLightStripsPlus
                | LM_Unknown !String

instance FromJSON LightModel where
    parseJSON (String s) =
        case T.unpack s of
            "LCT001" -> return LM_HueBulbA19
            "LCT007" -> return LM_HueBulbA19
            "LCT002" -> return LM_HueSpotBR30
            "LCT003" -> return LM_HueSpotGU10
            "LST001" -> return LM_HueLightStrips
            "LLC010" -> return LM_HueLivingColorsIris
            "LLC011" -> return LM_HueLivingColorsBloom
            "LLC012" -> return LM_HueLivingColorsBloom
            "LLC006" -> return LM_LivingColorsGen3Iris
            "LLC007" -> return LM_LivingColorsGen3BloomAura
            "LWB004" -> return LM_HueA19Lux
            "LWB006" -> return LM_HueA19Lux
            "LWB007" -> return LM_HueA19Lux
            "LLM001" -> return LM_ColorLightModule
            "LLM010" -> return LM_ColorTemperatureModule
            "LLM011" -> return LM_ColorTemperatureModule
            "LLM012" -> return LM_ColorTemperatureModule
            "LTW001" -> return LM_HueA19WhiteAmbience
            "LTW004" -> return LM_HueA19WhiteAmbience
            "LLC020" -> return LM_HueGo
            "LST002" -> return LM_HueLightStripsPlus
            str      -> return $ LM_Unknown str
    parseJSON _ = fail "Expected string"

instance Show LightModel where
    show LM_HueBulbA19                = "Hue Bulb A19"
    show LM_HueSpotBR30               = "Hue Spot BR30"
    show LM_HueSpotGU10               = "Hue Spot GU10"
    show LM_HueLightStrips            = "Hue Light Strips"
    show LM_HueLivingColorsIris       = "Hue Living Colors Iris"
    show LM_HueLivingColorsBloom      = "Hue Living Colors Bl."
    show LM_LivingColorsGen3Iris      = "Living Colors G3 Iris"
    show LM_LivingColorsGen3BloomAura = "Living Colors G3 B/A"
    show LM_HueA19Lux                 = "Hue A19 Lux"
    show LM_ColorLightModule          = "Color Light Module"
    show LM_ColorTemperatureModule    = "Color Temp. Module"
    show LM_HueA19WhiteAmbience       = "Hue A19 White Amb."
    show LM_HueGo                     = "Hue Go"
    show LM_HueLightStripsPlus        = "Hue Light Strips Plus"
    show (LM_Unknown s)               = "Unknown (" <> s <> ")"

-- Scenes
--
-- http://www.developers.meethue.com/documentation/scenes-api#41_get_all_scenes

data BridgeScene = BridgeScene { _bscName        :: !String
                               , _bscLights      :: ![LightID]
                               , _bscActive      :: !(Maybe Bool)
                               , _bscOwner       :: !(Maybe String)
                               , _bscRecycle     :: !(Maybe Bool)
                               , _bscLocked      :: !(Maybe Bool)
                               , _bscAppData     :: !(Maybe Object)
                               , _bscPicture     :: !(Maybe String)
                               , _bscLastUpdated :: !(Maybe UTCTime)
                               , _bscVersion     :: !(Maybe Int)
                               } deriving Show

-- Hue UTC strings miss the final Z, fails with the default parser
parseHueTimeMaybe :: Maybe String -> Maybe UTCTime
parseHueTimeMaybe Nothing = Nothing
parseHueTimeMaybe (Just t) =
    case parseTimeM True defaultTimeLocale "%FT%T" t of
        Just d -> Just d
        Nothing -> Nothing

instance FromJSON BridgeScene where
    parseJSON (Object o) = BridgeScene <$> o .:  "name"
                                       <*> o .:  "lights"
                                       <*> o .:? "active"
                                       <*> o .:? "owner"
                                       <*> o .:? "recycle"
                                       <*> o .:? "locked"
                                       <*> o .:? "appdata"
                                       <*> o .:? "picture"
                                       <*> (parseHueTimeMaybe <$> o .:? "lastupdated")
                                       <*> o .:? "version"
    parseJSON _ = fail "Expected object"

-- Bridge configuration obtained from the api/config endpoint without a whitelisted user

data BridgeConfigNoWhitelist = BridgeConfigNoWhitelist
    { _bcnwSWVersion  :: !String
    , _bcnwAPIVersion :: !APIVersion
    , _bcnwName       :: !String
    , _bcnwMac        :: !String
    } deriving Show

instance FromJSON BridgeConfigNoWhitelist where
    parseJSON (Object o) =
        BridgeConfigNoWhitelist <$> o .: "swversion"
                                <*> o .: "apiversion"
                                <*> o .: "name"
                                <*> o .: "mac"
    parseJSON _ = fail "Expected object"

-- API version string

data APIVersion = APIVersion { avMajor :: !Int, avMinor :: !Int, avPatch :: !Int }

instance FromJSON APIVersion where
    parseJSON (String s) =
        either (fail "Failed to parse version number")
               return
               (parseOnly parser s)
      where parser = APIVersion <$> (decimal <* char '.') <*> (decimal <* char '.') <*> decimal
    parseJSON _ = fail "Expected string"

instance Show APIVersion where
    show APIVersion { .. } = show avMajor <> "." <> show avMinor <> "." <> show avPatch

-- Actual bridge configuration obtainable by whitelisted user. We only parse a selection
-- of potentially interesting fields
--
-- http://www.developers.meethue.com/documentation/configuration-api#72_get_configuration

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

-- Some helper types for receiving lists / maps of the exported objects,
-- newtype wrappers for some modicum of type safety

newtype LightID = LightID { fromLightID :: String }
                  deriving (Eq, Ord, Show, FromJSON, ToJSON, Hashable)

newtype BridgeSceneID = BridgeSceneID { fromBridgeSceneID :: String }
                        deriving (Eq, Ord, Show, FromJSON, ToJSON, Hashable)

newtype GroupName = GroupName { fromGroupName :: String }
                    deriving (Eq, Ord, Show, FromJSON, ToJSON, Hashable)

type Lights       = HM.HashMap LightID Light                  -- Light ID to light
type LightGroups  = HM.HashMap GroupName (HS.HashSet LightID) -- Group names to set of light IDs
type BridgeScenes = HM.HashMap BridgeSceneID BridgeScene      -- Scene ID to scene

-- The newtype wrappers for the various string types give us problems with missing JSON
-- instances, just use coerce to safely reuse the ones we already got for plain String

instance FromJSON Lights where
    parseJSON v = (\(a :: HM.HashMap String Light) -> coerce a) <$> parseJSON v

instance FromJSON LightGroups where
    parseJSON v = (\(a :: HM.HashMap String (HS.HashSet LightID)) -> coerce a) <$> parseJSON v

instance FromJSON BridgeScenes where
    parseJSON v = (\(a :: HM.HashMap String BridgeScene) -> coerce a) <$> parseJSON v

-- Lenses

makeLenses ''BridgeConfigNoWhitelist
makeLenses ''BridgeConfig
makeLenses ''SWUpdate
makeLenses ''PortalState
makeLenses ''Light
makeLenses ''LightState
makeLenses ''BridgeScene

