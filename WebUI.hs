
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module WebUI ( webUIStart
             , LightUpdate(..)
             , LightUpdateTChan
             ) where

import Text.Printf
import Data.Monoid
import Data.List
import qualified Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Lens hiding ((#), set, (<.>), element)
import Control.Monad
import Control.Monad.Reader
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Trace
import AppDefs
import WebUIHelpers
import WebUITileBuilding

-- Threepenny based user interface for inspecting and controlling Hue devices

webUIStart :: MonadIO m => AppEnv -> m ()
webUIStart ae = do
    -- Start server
    let port = 8001
    traceS TLInfo $ "Starting web server on all interfaces, port " <> show port
    liftIO . startGUI
        defaultConfig { jsPort       = Just port
                      , jsAddr       = Just "0.0.0.0" -- All interfaces, not just loopback
                      , jsLog        = traceB TLInfo -- \_ -> return ()
                      , jsStatic     = Just "static"
                      , jsCustomHTML = Just "dashboard.html"
                      }
        $ setup ae

setup :: AppEnv -> Window -> UI ()
setup ae@AppEnv { .. } window =
  -- Run WebEnvUI monad
  flip runReaderT ae $ do
    -- Duplicate broadcast channel
    tchan <- liftIO . atomically $ dupTChan _aeBroadcast
    -- Title
    void . liftUI $ return window & set title "Hue Dashboard"

    -- TODO: Bootrap's JS features need jQuery, but the version included in threepenny
    --       is too old to be supported. It seems we can't use any of the JS features in
    --       Bootstrap until it is updated. Also see
    --       https://github.com/HeinrichApfelmus/threepenny-gui/issues/127
    --
    -- Bootstrap JS, should be in the body
    -- void $ getBody window #+
    --     [mkElement "script" & set (attr "src") ("static/bootstrap/js/bootstrap.min.js")]

    -- TODO: Page generation is very slow. Also see
    --       https://github.com/HeinrichApfelmus/threepenny-gui/issues/131

    -- TODO: Websocket connection can easily get dropped on mobile devices, maybe we could
    --       have some client side JS that just refreshes the page? Also see
    --       https://github.com/HeinrichApfelmus/threepenny-gui/issues/130

    -- Read all lights and light groups, display sorted by name. Light IDs in the group are
    -- already sorted by name
    (lights, lightGroupsList) <- liftIO . atomically $
        (,) <$> readTVar _aeLights
            <*> ( (sortBy (compare `Data.Function.on` fst) . HM.toList)
                  <$> readTVar _aeLightGroups
                )
    -- Root element where we insert all tiles
    root <- liftUI $ getElementByIdSafe window "lights"
    -- 'All Lights' tile
    addAllLightsTile window root
    -- Scenes tile
    addScenesTile window root
    -- Create tiles for all light groups
    forM_ lightGroupsList $ \(groupName, groupLightIDs) -> do
      -- Build group switch tile for current light group
      addGroupSwitchTile groupName groupLightIDs window root
      -- Create all light tiles for the current light group
      forM_ groupLightIDs $ \lightID -> case HM.lookup lightID lights of
        Nothing    -> return ()
        Just light -> addLightTile light lightID window root
    -- Worker thread for receiving light updates
    updateWorker <- liftIO . async $ lightUpdateWorker window tchan
    liftUI . on UI.disconnect window . const . liftIO $
        cancel updateWorker

-- Update DOM elements with light update messages received
--
-- TODO: We don't handle addition / removal of lights or changes in properties like the
--       name. Need to refresh page for those to show up
--
-- TODO: Because getElementById just freezes when we pass it a non-existent element, our
--       entire worker thread will just freeze when we receive an update for a new light,
--       or one with a changed ID etc., very bad, see getElementByIdSafe
--
lightUpdateWorker :: Window -> LightUpdateTChan -> IO ()
lightUpdateWorker window tchan = runUI window $ loop
  where
    loop = do
      (liftIO . atomically $ readTChan tchan) >>=
        \(lightID, update) -> case update of
          -- Light turned on / off
          LU_OnOff s ->
            getElementByIdSafe window (buildID lightID "tile") >>= \e ->
                void $ return e & set style [if s then enabledOpacity else disabledOpacity]
          -- All lights off, grey out 'All Lights' tile
          LU_LastOff ->
            getElementByIdSafe window (buildID "all-lights" "tile") >>= \e ->
              void $ return e & set style [disabledOpacity]
          -- At least one light on, activate 'All Lights' tile
          LU_FirstOn ->
            getElementByIdSafe window (buildID "all-lights" "tile") >>= \e ->
              void $ return e & set style [enabledOpacity]
          -- All lights in a group off, grey out group switch tile
          LU_GroupLastOff grp ->
            getElementByIdSafe window (buildID ("group-" <> grp) "tile") >>= \e ->
              void $ return e & set style [disabledOpacity]
          -- At least one light in a group on, activate group switch tile
          LU_GroupFirstOn grp ->
            getElementByIdSafe window (buildID ("group-" <> grp) "tile") >>= \e ->
              void $ return e & set style [enabledOpacity]
          -- Brightness change
          LU_Brightness brightness -> do
            let brightPercent = printf "%.0f%%" (fromIntegral brightness * 100 / 255 :: Float)
            getElementByIdSafe window (buildID lightID "brightness-bar") >>= \e ->
              void $ return e & set style [("width", brightPercent)]
            getElementByIdSafe window (buildID lightID "brightness-percentage") >>= \e ->
              void $ return e & set UI.text brightPercent
          -- Color change
          LU_Color col ->
            getElementByIdSafe window (buildID lightID "image") >>= \e ->
              void $ return e & set style [("background", col)]
      loop

