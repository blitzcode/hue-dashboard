
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module WebUIHelpers where

import Data.Monoid
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Control.Lens hiding ((<.>))
import Control.Monad.Reader
import Graphics.UI.Threepenny.Core

import HueJSON
import AppDefs (AppEnv)

-- Some utility functions split out from the WebUI / TileBuilding modules

-- Opacities used for enabled and disabled elements
enabledOpacity, disabledOpacity :: (String, String)
enabledOpacity  = ("opacity", "1.0")
disabledOpacity = ("opacity", "0.3")

-- Amount of brightness changed when any brightness widget is used
brightnessChange :: Int
brightnessChange = 25 -- Relative to 255

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
    -- liftIO . putStrLn $ "getElementByIdSafe: " <> elementID
    fromJust <$> getElementById window elementID

-- TODO: Those any* functions duplicate functionality already have in App.fetchBridgeState

anyLightsOn :: Lights -> Bool
anyLightsOn lights = any (^. _2 . lgtState . lsOn) $ HM.toList lights

anyLightsInGroup :: String -> LightGroups -> Lights -> (Light -> Bool) -> Bool
anyLightsInGroup groupID groups lights condition =
    case HM.lookup groupID groups of
        Nothing          -> False
        Just groupLights ->
            or . map condition . catMaybes . map (flip HM.lookup lights) $ groupLights

