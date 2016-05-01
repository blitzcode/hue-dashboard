
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module WebUITileBuilding ( addLightTile
                         , addGroupSwitchTile
                         , addAllLightsTile
                         , addScenesTile
                         ) where

import Text.Printf
import Data.Monoid
import Data.List
import qualified Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import Control.Lens hiding ((#), set, (<.>), element)
import Control.Monad
import Control.Monad.Reader
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified Codec.Picture as JP
import System.FilePath
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import HueJSON
import LightColor
import AppDefs
import PersistConfig
import WebUIHelpers
import WebUIREST

-- Code for building the individual tiles making up our user interface

addLightTile :: Light -> String -> Window -> PageBuilder ()
addLightTile light lightID window = do
  AppEnv { .. } <- ask
  -- Build tile
  let opacity       = if light ^. lgtState ^. lsOn then enabledOpacity else disabledOpacity
      brightPercent = printf "%.0f%%"
                        ( fromIntegral (light ^. lgtState . lsBrightness . non 255)
                          * 100 / 255 :: Float
                        ) :: String
      colorStr      = htmlColorFromLight light
      colorSupport  = isColorLT $ light ^. lgtType
  addPageTile $
    H.div H.! A.class_ "thumbnail"
          H.! A.style (H.toValue $ "opacity: " <> show opacity <> ";")
          H.! A.id (H.toValue $ buildID lightID "tile") $ do
      -- Caption and light icon
      H.div H.! A.class_ "light-caption small"
            H.! A.id (H.toValue $ buildID lightID "caption") $
              H.toHtml $ light ^. lgtName
      H.img H.! A.class_ "img-rounded"
            H.! A.style (H.toValue $ "background: " <> colorStr <> ";")
            H.! A.src (H.toValue . iconFromLM $ light ^. lgtModelID)
            H.! A.id (H.toValue $ buildID lightID "image")
      -- Only add color picker elements for lights that support colors
      when colorSupport $
          addColorPicker lightID
      -- Model type and text
      H.div H.! A.class_ "text-center" $
        H.h6 $
          H.small $ do
            H.toHtml . show $ light ^. lgtModelID
            H.br
            H.toHtml . show $ light ^. lgtType
      -- Brightness widget
      H.div H.! A.class_ "progress"
            H.! A.id (H.toValue $ buildID lightID "brightness-container") $ do
        H.div H.! A.class_ "progress-label-container" $ do
          H.div H.! A.class_ "glyphicon glyphicon-minus minus-label" $ return ()
          H.div H.! A.class_ "glyphicon glyphicon-plus plus-label" $ return ()
          H.div H.! A.class_ "percentage-label" $
            H.small $
              H.span H.! A.id (H.toValue $ buildID lightID "brightness-percentage") $
                H.toHtml brightPercent
        H.div H.! A.class_ "progress-bar progress-bar-info"
              H.! A.style (H.toValue $ "width: " <> brightPercent <> ";")
              H.! A.id (H.toValue $ buildID lightID "brightness-bar")
              $ return ()
  addPageUIAction $ do
     -- Have light blink once after clicking the caption
     getElementByIdSafe window (buildID lightID "caption") >>= \caption ->
         on UI.click caption $ \_ -> do
             lightsBreatheCycle (_aePC ^. pcBridgeIP)
                                (_aePC ^. pcUserID)
                                [lightID]
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
     -- Respond to clicks on the color picker
     when colorSupport $
         getElementByIdSafe window (buildID lightID "color-picker-overlay") >>= \image ->
             on UI.mousedown image $ \(mx, my) ->
                 case xyFromColorPickerCoordinates _aeColorPickerImg mx my (light ^. lgtModelID) of
                     CPR_Margin          -> return ()
                     CPR_EnableColorLoop ->
                         lightsColorLoop (_aePC ^. pcBridgeIP)
                                         (_aePC ^. pcUserID)
                                         _aeLights
                                         [lightID]
                     CPR_XY xyX xyY      ->
                         lightsSetColorXY (_aePC ^. pcBridgeIP)
                                          (_aePC ^. pcUserID)
                                          _aeLights
                                          [lightID]
                                          xyX
                                          xyY

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

-- Build group switch tile for current light group
addGroupSwitchTile :: String -> [String] -> Window -> PageBuilder ()
addGroupSwitchTile groupName groupLightIDs window = do
  AppEnv { .. } <- ask
  let groupID                         = "group-" <> groupName
      queryAnyLightsInGroup condition =
        (liftIO . atomically $ (,) <$> readTVar _aeLights <*> readTVar _aeLightGroups)
          >>= \(lights, lightGroups) -> return $
            anyLightsInGroup groupName lightGroups lights condition
  grpHasColor <- queryAnyLightsInGroup (^. lgtType . to isColorLT)
  -- Tile
  queryAnyLightsInGroup (^. lgtState . lsOn) >>= \grpOn ->
    addPageTile $
      H.div H.! A.class_ "thumbnail"
            H.! A.style ( H.toValue $ "opacity: "
                            <> show (if grpOn then enabledOpacity else disabledOpacity)
                            <> ";"
                        )
            H.! A.id (H.toValue $ buildID groupID "tile") $ do
        -- Caption and switch icon
        H.div H.! A.class_ "light-caption light-caption-group-header small"
              H.! A.id (H.toValue $ buildID groupID "caption") $ do
                (void "Group Switch")
                H.br
                H.toHtml groupName
        H.img H.! A.class_ "img-rounded"
              H.! A.src "static/svg/hds.svg"
              H.! A.id (H.toValue $ buildID groupID "image")
        -- Only add color picker elements for lights that support colors
        when grpHasColor $
            addColorPicker groupID
        -- Group description
        H.div H.! A.class_ "text-center" $
          H.h6 $
            H.small $ do
              H.toHtml $ ((show $ length groupLightIDs) <> " Light(s)")
              H.br
              "(Grouped by Prefix)"
        -- Brightness widget
        H.div H.! A.class_ "progress"
              H.! A.id (H.toValue $ buildID groupID "brightness-container") $ do
          H.div H.! A.class_ "progress-label-container" $ do
            H.div H.! A.class_ "glyphicon glyphicon-minus minus-label" $ return ()
            H.div H.! A.class_ "glyphicon glyphicon-plus plus-label"   $ return ()
          H.div H.! A.class_   "progress-bar progress-bar-info"        $ return ()
  addPageUIAction $ do
      -- Have light blink once after clicking the caption
      getElementByIdSafe window (buildID groupID "caption") >>= \caption ->
          on UI.click caption $ \_ -> do
              lightsBreatheCycle (_aePC ^. pcBridgeIP)
                                 (_aePC ^. pcUserID)
                                 groupLightIDs
      -- Register click handler for turning group lights on / off
      getElementByIdSafe window (buildID groupID "image") >>= \image ->
          on UI.click image $ \_ -> do
              -- Query current group light state to see if we need to turn group on or off
              queryAnyLightsInGroup  (^. lgtState . lsOn)>>= \grpOn ->
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
      -- Respond to clicks on the color picker
      when grpHasColor $
          getElementByIdSafe window (buildID groupID "color-picker-overlay") >>= \image ->
              on UI.mousedown image $ \(mx, my) ->
                  -- TODO: We have to specify a single light type for the color conversion,
                  --       but we potentially set many different lights. Do a custom
                  --       conversion for each color light in the group
                  case xyFromColorPickerCoordinates _aeColorPickerImg mx my LM_HueBulbA19 of
                      CPR_Margin          -> return ()
                      CPR_EnableColorLoop ->
                          lightsColorLoop (_aePC ^. pcBridgeIP)
                                          (_aePC ^. pcUserID)
                                          _aeLights
                                          groupLightIDs
                      CPR_XY xyX xyY      ->
                          lightsSetColorXY (_aePC ^. pcBridgeIP)
                                           (_aePC ^. pcUserID)
                                           _aeLights
                                           groupLightIDs
                                           xyX
                                           xyY

-- Tile for controlling all lights, also displays some bridge information
addAllLightsTile :: Window -> PageBuilder ()
addAllLightsTile window = do
  AppEnv { .. } <- ask
  -- Build tile
  void $ do
    lights <- liftIO . atomically $ readTVar _aeLights
    let lgtOn = anyLightsOn lights
    addPageTile $
      H.div H.! A.class_ "thumbnail"
            H.! A.style ( H.toValue $ "opacity: "
                            <> show (if lgtOn then enabledOpacity else disabledOpacity)
                            <> ";"
                        )
            H.! A.id (H.toValue $ buildID "all-lights" "tile") $ do
        H.div H.! A.class_ "light-caption light-caption-group-header small" $ "All Lights"
        H.img H.! A.class_ "img-rounded"
              H.! A.src "static/svg/bridge_v2.svg"
              H.! A.id (H.toValue $ buildID "all-lights" "image")
        H.div H.! A.class_ "text-center" $
          H.h6 $
            H.small $
              sequence_ $ intersperse H.br
                [ H.toHtml $ "Model " <> (_aeBC ^. bcModelID)
                , H.toHtml $ "IP "    <> (_aePC ^. pcBridgeIP)
                , H.toHtml $ "API v"  <> (show $ _aeBC ^. bcAPIVersion)
                , H.toHtml $ (show $ length lights) <> " Lights Connected"
                ]
  -- Register click handler for turning all lights on / off
  addPageUIAction $
      getElementByIdSafe window (buildID "all-lights" "image") >>= \image ->
          on UI.click image $ \_ -> do
              -- Query current light state to see if we need to turn everything on or off
              lights <- liftIO . atomically $ readTVar _aeLights
              -- Fire & forget REST API call in another thread
              switchAllLights (_aePC ^. pcBridgeIP)
                              (_aePC ^. pcUserID)
                              (not $ anyLightsOn lights)

addScenesTile :: Window -> PageBuilder ()
addScenesTile window = do
  AppEnv { .. } <- ask
  let sceneBttnID sceneID = -- TODO: Move this logic to where the scenes are fetched
        -- DOM ID from scene ID
        "scene-activate-bttn-" <> sceneID
      nameKeyedScenes =
          -- Use the scene name as the key instead of the scene ID
          map (\(sceneID, scene) -> (scene ^. scName, (sceneID, scene)))
              $ HM.toList _aeScenes
      nubScenes =
          -- Build 'name -> (sceneID, scene)' hashmap, resolve name
          -- collisions with the last update date
          flip HM.fromListWith nameKeyedScenes $ \sceneA sceneB ->
              case (compare `Data.Function.on` (^. _2 . scLastUpdated)) sceneA sceneB of
                  EQ -> sceneA
                  LT -> sceneB
                  GT -> sceneA
      recentScenes =
          -- List of scenes sorted by last update date
          reverse . sortBy (compare `Data.Function.on` (^. _2 . scLastUpdated)) .
              map snd $ HM.toList nubScenes
      fixNames =
          -- Scene names are truncated and decorated when stored on the bridge,
          -- salvage what we can and extract the cleanest UI label for them
          recentScenes & traversed . _2 . scName %~ \sceneName ->
              (\nm -> if length nm == 16 then nm <> "…" else nm) . take 16 .
                  concat . intersperse " " . reverse $ case reverse $ words sceneName of
                      xs@("0":"on":_)  -> drop 2 xs
                      xs@("on":_)      -> drop 1 xs
                      xs@("0":"off":_) -> drop 2 xs
                      xs@("off":_)     -> drop 1 xs
                      xs               -> xs
      topScenes = take 8 fixNames
  -- Build scenes tile
  addPageTile $
    H.div H.! A.class_ "thumbnail" $ do
      H.div H.! A.class_ "light-caption light-caption-group-header small" $ "Recent Scenes"
      H.div H.! A.class_ "btn-group-vertical btn-group-xs scene-btn-group" $
        forM_ topScenes $ \(sceneID, scene) ->
          H.button H.! A.class_ "btn btn-scene"
                   H.! A.id (H.toValue $ sceneBttnID sceneID) $
                     H.small $ (H.toHtml $ scene ^. scName)
  -- Register click handlers for activating the scenes
  addPageUIAction $
      forM_ topScenes $ \(sceneID, _) ->
          getElementByIdSafe window (sceneBttnID sceneID) >>= \bttn ->
              on UI.click bttn $ \_ ->
                  recallScene (_aePC ^. pcBridgeIP)
                              (_aePC ^. pcUserID)
                              sceneID

data ColorPickerResult = CPR_Margin          -- Click on the margin
                       | CPR_EnableColorLoop -- Click on the 'Enable Color Loop' button
                       | CPR_XY Float Float  -- Clicked on the color / color temperature parts, XY

-- Classify results from a click on the color picker image
xyFromColorPickerCoordinates :: JP.Image JP.PixelRGB8
                             -> Int
                             -> Int
                             -> LightModel
                             -> ColorPickerResult
xyFromColorPickerCoordinates colorPickerImg mx' my' lm =
    let wdh    = JP.imageWidth  colorPickerImg
        hgt    = JP.imageHeight colorPickerImg
        margin = 10 -- There's a margin around the image on the website
        button = 40 -- The 'Enable Color Loop' button is on the bottom
        mx     = mx' - margin
        my     = my' - margin
    in  case () of
            _ | mx < 0 || my < 0 || mx >= wdh || my >= hgt -> CPR_Margin
              | my >= hgt - button                         -> CPR_EnableColorLoop
              | otherwise                                  ->
                    -- Look up color in color picker image, convert to XY
                    let (JP.PixelRGB8 r g b) = JP.pixelAt colorPickerImg mx my
                        (xyX, xyY)           = rgbToXY ( fromIntegral r / 255
                                                       , fromIntegral g / 255
                                                       , fromIntegral b / 255
                                                       )
                                               lm
                    in  CPR_XY xyX xyY

-- Add color picker and 'tint' button
addColorPicker :: String -> H.Html
addColorPicker grpOrLgtID = do
  H.div H.! A.style "display: none;"
        H.! A.id (H.toValue $ buildID grpOrLgtID "color-picker-container") $ do
    H.div H.! A.class_ "color-picker-curtain"
          H.! A.onclick "this.parentNode.style.display = 'none'"
          $ return ()
    H.img H.! A.class_ "color-picker-overlay"
          H.! A.src "static/color_picker.png"
          H.! A.id (H.toValue $ buildID grpOrLgtID "color-picker-overlay")
  H.div H.! A.class_ "color-picker-button"
        H.! A.onclick
          -- Click button to make color picker visible, but not
          -- for tiles that are turned off (opacity < 1)
          --
          -- TODO: Glitches when a tile is switched off while
          --       the color picker is open, just move curtain
          --       and overlay out of the tile so their opacity
          --       is not affected
          --
          ( H.toValue $ ( printf ( "if (getElementById('%s').style.opacity == 1) " ++
                                   " { getElementById('%s').style.display = 'block'; }"
                                 )
                          (buildID grpOrLgtID "tile")
                          (buildID grpOrLgtID "color-picker-container")
                          :: String
                        )
          ) $
    H.div H.! A.class_ "glyphicon glyphicon-tint color-picker-tint-icon" $ return ()

