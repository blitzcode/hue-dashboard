
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module WebUIHelpers where

import Data.Monoid
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Lens hiding ((<.>))
import Control.Monad
import Control.Monad.Reader
import Graphics.UI.Threepenny.Core
import System.FilePath

import Util
import HueJSON
import HueREST
import AppDefs

-- Some utility functions split out from the WebUI module

-- Run a reader with the application environment on top of the UI monad
type WebEnvUI = ReaderT AppEnv UI

-- Lift threepenny's UI monad into ours
liftUI :: UI a -> WebEnvUI a
liftUI = lift

-- Build a string for the id field in a light specific DOM object. Do this in one
-- place as we need to locate them later when we want to update
buildID :: String -> String -> String
buildID lightID elemName = "light-" <> lightID <> "-" <> elemName

-- The getElementById function returns a Maybe, but actually just throws an exception if
-- the element is not found. The exception is unfortunately a JS exception on the client,
-- and our code just freezes / aborts without any helpful reason why the page couldn't be
-- generated. Until this is fixed in threepenny, we can only add support for tracing. Also
-- see https://github.com/HeinrichApfelmus/threepenny-gui/issues/129
getElementByIdSafe :: Window -> String -> UI Element
getElementByIdSafe window elementID = do
    -- traceS TLInfo $ "getElementByIdSafe: " <> elementID
    fromJust <$> getElementById window elementID

iconFromLM :: LightModel -> FilePath
iconFromLM lm = basePath </> fn <.> ext
  where
    basePath = "static/svg"
    ext      = "svg"
    fn       = case lm of LM_HueBulbA19                -> "white_and_color_e27"
                          LM_HueSpotBR30               -> "br30"
                          LM_HueSpotGU10               -> "gu10"
                          LM_HueLightStrips            -> "lightstrip"
                          LM_HueLivingColorsIris       -> "iris"
                          LM_HueLivingColorsBloom      -> "bloom"
                          LM_LivingColorsGen3Iris      -> "iris"
                          LM_LivingColorsGen3BloomAura -> "bloom"
                          LM_HueA19Lux                 -> "white_e27"
                          LM_ColorLightModule          -> "white_and_color_e27"
                          LM_ColorTemperatureModule    -> "white_e27"
                          LM_HueGo                     -> "go"
                          LM_HueLightStripsPlus        -> "lightstrip"
                          LM_Unknown _                 -> "white_e27"

-- We make a REST- API call in another thread to change the state on the bridge. The call
-- is fire & forget, we don't retry in case of an error

-- http://www.developers.meethue.com/documentation/lights-api#16_set_light_state

lightsSwitchOnOff :: MonadIO m => IPAddress -> String -> [String] -> Bool -> m ()
lightsSwitchOnOff bridgeIP userID lightIDs onOff =
    void . liftIO . async $
        forM_ lightIDs $ \lightID ->
            bridgeRequestTrace
                MethodPUT
                bridgeIP
                (Just $ HM.fromList [("on" :: String, onOff)])
                userID
                ("lights" </> lightID </> "state")

lightsChangeBrightness :: MonadIO m => IPAddress -> String -> TVar Lights -> [String] -> Int -> m ()
lightsChangeBrightness bridgeIP userID lights' lightIDs change = do
    -- First check which of the lights we got are turned on. Changing the brightness
    -- of a light in the off state will just result in an error response
    lights <- liftIO . atomically $ readTVar lights'
    let onLightIDs = filter (maybe False (^. lgtState . lsOn) . flip HM.lookup lights) lightIDs
    void . liftIO . async $
        forM_ onLightIDs $ \lightID ->
            bridgeRequestTrace
                MethodPUT
                bridgeIP
                (Just $ HM.fromList [("bri_inc" :: String, change)])
                userID
                ("lights" </> lightID </> "state")

-- http://www.developers.meethue.com/documentation/groups-api#253_body_example

recallScene :: MonadIO m => IPAddress -> String -> String -> m ()
recallScene bridgeIP userID sceneID =
    void . liftIO . async $
        bridgeRequestTrace
            MethodPUT
            bridgeIP
            (Just $ HM.fromList [("scene" :: String, sceneID)])
            userID
            ("groups/0/action")

-- http://www.developers.meethue.com/documentation/groups-api#25_set_group_state

switchAllLights :: MonadIO m => IPAddress -> String -> Bool -> m ()
switchAllLights bridgeIP userID onOff =
    let body = HM.fromList[("on" :: String, onOff)]
    in  void . liftIO . async $
            bridgeRequestTrace
                MethodPUT
                bridgeIP
                (Just body)
                userID
                ("groups" </> "0" </> "action") -- Special group 0, all lights

-- TODO: Those any* functions duplicate functionality already have in App.fetchBridgeState

anyLightsOn :: Lights -> Bool
anyLightsOn lights = any (^. _2 . lgtState . lsOn) $ HM.toList lights

anyLightsInGroupOn :: String -> LightGroups -> Lights -> Bool
anyLightsInGroupOn groupID groups lights =
    case HM.lookup groupID groups of
        Nothing          -> False
        Just groupLights -> or . map (^. lgtState . lsOn) .
                                catMaybes . map (flip HM.lookup lights) $ groupLights

