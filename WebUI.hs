
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module WebUI ( webUIStart
             , LightUpdate(..)
             , LightUpdateTChan
             ) where

import Text.Printf
import Data.Monoid
import Data.List
import Data.Maybe
import qualified Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Lens hiding ((#), set, (<.>), element)
import Control.Monad
import Control.Monad.IO.Class
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.FilePath

import Util
import Trace
import HueJSON
import HueREST
import LightColor
import AppDefs
import PersistConfig

-- Threepenny based user interface for inspecting and controlling Hue devices

-- Opacities used for enabled and disabled elements
enabledOpacity, disabledOpacity :: (String, String)
enabledOpacity  = ("opacity", "1.0")
disabledOpacity = ("opacity", "0.3")

-- Amount of brightness changed when any brightness widget is used
brightnessChange :: Int
brightnessChange = 25

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
setup AppEnv { .. } window = do
    -- Duplicate broadcast channel
    tchan <- liftIO . atomically $ dupTChan _aeBroadcast
    -- Title
    void $ return window # set title "Hue Dashboard"

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

    -- TODO: Add tile for recalling scenes. Might be tricky, the scenes from the official
    --       app only seem to be 'cached' on the bridge with incomplete names and duplicate
    --       versions etc.

    -- Root element where we insert all tiles
    root <- getElementByIdSafe window "lights"
    -- Read all lights and light groups, display sorted by name. Light IDs in the group are
    -- already sorted by name
    (lights, lightGroups) <- liftIO . atomically $
        (,) <$> readTVar _aeLights
            <*> ( (sortBy (compare `Data.Function.on` fst) . HM.toList)
                  <$> readTVar _aeLightGroups
                )
    -- 'All Lights' tile
    anyLightsOn _aeLights >>= \lgtOn ->
      void $ element root #+
        [ ( UI.div #. "thumbnail" & set style [if lgtOn then enabledOpacity else disabledOpacity]
                                  & set UI.id_ (buildID "all-lights" "tile")
          ) #+
          [ UI.div #. "light-caption light-caption-group-header small" #+ [string "All Lights"]
          , UI.img #. "img-rounded" & set UI.src "static/svg/bridge_v2.svg"
                                    & set UI.id_ (buildID "all-lights" "image")
          , UI.div #. "text-center" #+
            [ UI.h6 #+
              [ UI.small #+ intersperse UI.br
                -- Also use this tile to show some information on the bridge
                [ string "Hue Bridge"
                , string $ "Model " <> (_aeBC ^. bcModelID)
                , string $ "IP "    <> (_aePC ^. pcBridgeIP)
                , string $ "API v"  <> (show $ _aeBC ^. bcAPIVersion)
                , string $ (show $ length lights) <> " Lights Connected"
                ]
              ]
            ]
          ]
        ]
    -- Register click handler for turning all lights on / off
    getElementByIdSafe window (buildID "all-lights" "image") >>= \image ->
        on UI.click image $ \_ -> do
            -- Query current light state to see if we need to turn everything on or off
            lgtOn <- anyLightsOn _aeLights
            let body = HM.fromList[("on" :: String, if lgtOn then False else True)]
            -- Fire & forget REST API call in another thread
            -- http://www.developers.meethue.com/documentation/groups-api#25_set_group_state
            void . liftIO . async $
                bridgeRequestTrace
                    MethodPUT
                    (_aePC ^. pcBridgeIP)
                    (Just body)
                    (_aePC ^. pcUserID)
                    ("groups" </> "0" </> "action") -- Special group 0, all lights
    -- Create tiles for all light groups
    forM_ lightGroups $ \(groupName, groupLightIDs) -> do
      -- Build group switch tile for current light group
      let groupID = "group-" <> groupName
      anyLightsInGroupOn groupName _aeLightGroups _aeLights >>= \grpOn ->
        void $ element root #+
          [ ( UI.div #. "thumbnail" & set style [if grpOn then enabledOpacity else disabledOpacity]
                                    & set UI.id_ (buildID groupID "tile")
            ) #+
            [ UI.div #. "light-caption light-caption-group-header small" #+
              [ string "Group Switch"
              , UI.br
              , string groupName
              ]
            , UI.img #. "img-rounded" & set UI.src "static/svg/hds.svg"
                                      & set UI.id_ (buildID groupID "image")
            , UI.div #. "text-center" #+
              [ UI.h6 #+
                [ UI.small #+
                  [ string $ ((show $ length groupLightIDs) <> " Light(s)")
                  , UI.br
                  , string "(Grouped by Prefix)"
                  ]
                ]
              ]
            , UI.div #. "small text-center" #+ [string "Brightness"]
            , ( UI.div #. "progress"
                        & set UI.id_ (buildID groupID "brightness-container")
              ) #+
              [ ( UI.div #. "progress-label-container") #+
                [ UI.div #. "glyphicon glyphicon-minus minus-label"
                , UI.div #. "glyphicon glyphicon-plus plus-label"
                ]
              , UI.div #. "progress-bar progress-bar-info"
              ]
            ]
          ]
      -- Register click handler for turning group lights on / off
      getElementByIdSafe window (buildID groupID "image") >>= \image ->
          on UI.click image $ \_ -> do
              -- Query current group light state to see if we need to turn group on or off
              grpOn <- anyLightsInGroupOn groupName _aeLightGroups _aeLights
              lightsSwitchOnOff (_aePC ^. pcBridgeIP)
                                (_aePC ^. pcUserID)
                                groupLightIDs
                                (not grpOn)
      -- Register click handler for changing group brightness
      getElementByIdSafe window (buildID groupID "brightness-container") >>= \image ->
          on UI.mousedown image $ \(mx, _) ->
              -- Construct and perform REST API call
              lightsChangeBrightness (_aePC ^. pcBridgeIP)
                                     (_aePC ^. pcUserID)
                                     _aeLights
                                     groupLightIDs
                                     -- Click on left part decrements, right part increments
                                     (if mx < 50 then (-brightnessChange) else brightnessChange)
      -- Create all light tiles for the current light group
      forM_ groupLightIDs $ \lightID -> case HM.lookup lightID lights of
        Nothing    -> return ()
        Just light -> do
          -- Build HTML for tile
          --
          -- TODO: This of course duplicates some code from the update routines, maybe just
          --       build the skeleton here and let the updating set all actual state?
          let opacity       = if light ^. lgtState ^. lsOn then enabledOpacity else disabledOpacity
              brightPercent = printf "%.0f%%"
                                ( fromIntegral (light ^. lgtState . lsBrightness . non 255)
                                  * 100 / 255 :: Float
                                )
              colorStr      = htmlColorFromRGB . colorFromLight $ light
           in void $ element root #+
                [ ( UI.div #. "thumbnail" & set style [opacity]
                                          & set UI.id_ (buildID lightID "tile")
                  ) #+
                  [ UI.div #. "light-caption small" #+ [string $ light ^. lgtName]
                  , UI.img #. "img-rounded" & set style [ ("background", colorStr)]
                                            & set UI.src (iconFromLM $ light ^. lgtModelID)
                                            & set UI.id_ (buildID lightID "image")
                  , UI.div #. "text-center" #+
                    [ UI.h6 #+
                      [ UI.small #+
                        [ string $ (show $ light ^. lgtModelID)
                        , UI.br
                        , string $ (show $ light ^. lgtType)
                        ]
                      ]
                    ]
                  , UI.div #. "small text-center" #+ [string "Brightness"]
                  , ( UI.div #. "progress"
                              & set UI.id_ (buildID lightID "brightness-container")
                    ) #+
                    [ ( UI.div #. "progress-label-container") #+
                      [ UI.div #. "glyphicon glyphicon-minus minus-label"
                      , UI.div #. "glyphicon glyphicon-plus plus-label"
                      , UI.div #. "percentage-label" #+
                        [ UI.small #+
                          [ string brightPercent & set UI.id_
                                                       (buildID lightID "brightness-percentage")
                          ]
                        ]
                      ]
                    , ( UI.div #. "progress-bar progress-bar-info"
                                & set style [("width", brightPercent)]
                                & set UI.id_ (buildID lightID "brightness-bar")
                      )
                    ]
                  ]
                ]
          -- Register click handlers for the on / off and brightness controls
          --
          -- TODO: Add UI and handlers for changing color (also try color loop mode)
          --
          -- Turn on / off by clicking the light symbol
          getElementByIdSafe window (buildID lightID "image") >>= \image ->
              on UI.click image $ \_ -> do
                  -- Query current light state to see if we need to turn it on or off
                  curLights <- liftIO . atomically $ readTVar _aeLights
                  case HM.lookup lightID curLights of
                      Nothing         -> return ()
                      Just lightOnOff -> lightsSwitchOnOff (_aePC ^. pcBridgeIP)
                                                           (_aePC ^. pcUserID)
                                                           [lightID]
                                                           (not $ lightOnOff ^. lgtState . lsOn)
          -- Change brightness bright clicking the left / right side of the brightness bar
          getElementByIdSafe window (buildID lightID "brightness-container") >>= \image ->
              on UI.mousedown image $ \(mx, _) ->
                  -- Construct and perform REST API call
                  lightsChangeBrightness (_aePC ^. pcBridgeIP)
                                         (_aePC ^. pcUserID)
                                         _aeLights
                                         [lightID]
                                         -- Click on left part decrements, right part increments
                                         (if mx < 50 then (-brightnessChange) else brightnessChange)
    -- Worker thread for receiving light updates
    updateWorker <- liftIO . async $ lightUpdateWorker window tchan
    on UI.disconnect window . const . liftIO $
        cancel updateWorker

-- We make a REST- API call in another thread to change the state on the bridge. The call
-- is fire & forget, we don't retry in case of an error
--
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

-- TODO: Those any* functions duplicate functionality already have in App.fetchBridgeState

anyLightsOn :: MonadIO m => TVar Lights -> m Bool
anyLightsOn lights' =
    liftIO . atomically $ HM.toList <$> readTVar lights'
        >>= return . any (^. _2 . lgtState . lsOn)

anyLightsInGroupOn :: MonadIO m => String -> TVar LightGroups -> TVar Lights -> m Bool
anyLightsInGroupOn groupID groups' lights' = do
    (groups, lights) <- liftIO . atomically $ (,) <$> readTVar groups' <*> readTVar lights'
    case HM.lookup groupID groups of
        Nothing          -> return False
        Just groupLights -> return . or . map (^. lgtState . lsOn) .
                                catMaybes . map (flip HM.lookup lights) $ groupLights

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
          LU_Color rgb ->
            getElementByIdSafe window (buildID lightID "image") >>= \e ->
              void $ return e & set style [("background", htmlColorFromRGB rgb)]
      loop

-- Build a string for the id field in a light specific DOM object. Do this in one
-- place as we need to locate them later when we want to update
buildID :: String -> String -> String
buildID lightID elemName = "light-" <> lightID <> "-" <> elemName

-- The getElementById function returns a Maybe, but actually just throws an exception if
-- the element is not found. The exception is unfortunately a JS exception on the client,
-- and our code just freezes / aborts without any helpful reason why the page couldn't be
-- generated. Until this is fixed in threepenny, we can only add support for tracing. Also
-- see https://github.com/HeinrichApfelmus/threepenny-gui/issues/129
--
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

