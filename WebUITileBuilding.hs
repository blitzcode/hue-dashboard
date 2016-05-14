
{-# LANGUAGE OverloadedStrings, RecordWildCards, RankNTypes, LambdaCase #-}

module WebUITileBuilding ( addLightTile
                         , addGroupSwitchTile
                         , addAllLightsTile
                         , addServerTile
                         ) where

import Text.Printf
import Data.Monoid
import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Control.Concurrent.STM
import Control.Lens hiding ((#), set, (<.>), element)
import Control.Monad
import Control.Monad.Reader
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified Codec.Picture as JP
import System.FilePath
import System.Random
import System.Process
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Util
import HueJSON
import LightColor
import AppDefs
import PersistConfig
import WebUIHelpers
import WebUIREST
import ProcFS

-- Code for building the individual tiles making up our user interface

-- Add tile for an individual light
--
-- TODO: Add ability to rename lights
--
addLightTile :: Light -> LightID -> Bool -> Window -> PageBuilder ()
addLightTile light lightID shown window = do
  AppEnv { .. } <- ask
  -- Get relevant bridge information, assume it won't change over the lifetime of the connection
  bridgeIP     <- liftIO . atomically $ (^. pcBridgeIP    ) <$> readTVar _aePC
  bridgeUserID <- liftIO . atomically $ (^. pcBridgeUserID) <$> readTVar _aePC
  -- Build tile
  let opacity       = if light ^. lgtState . lsOn then enabledOpacity else disabledOpacity
      brightPercent = printf "%.0f%%"
                        ( fromIntegral (light ^. lgtState . lsBrightness . non 255)
                          * 100 / 255 :: Float
                        ) :: String
      colorStr      = htmlColorFromLightState $ light ^. lgtState
      colorSupport  = isColorLT $ light ^. lgtType
  addPageTile $
    H.div H.! A.class_ "tile"
          H.! A.style ( H.toValue $ "opacity: " <> show opacity <> ";" <>
                                    if shown then "display: block;" else "display: none;"
                      )
          H.! A.id (H.toValue $ buildLightID lightID "tile") $ do
      -- Caption and light icon
      H.div H.! A.class_ "light-caption small"
            H.! A.id (H.toValue $ buildLightID lightID "caption") $
              H.toHtml $ light ^. lgtName
      H.img H.! A.class_ "img-rounded"
            H.! A.style (H.toValue $ "background: " <> colorStr <> ";")
            H.! A.src (H.toValue . iconFromLM $ light ^. lgtModelID)
            H.! A.id (H.toValue $ buildLightID lightID "image")
      -- Only add color picker elements for lights that support colors
      when colorSupport $
              addColorPicker (buildLightID lightID "tile"                  )
                             (buildLightID lightID "color-picker-container")
                             (buildLightID lightID "color-picker-overlay"  )
      -- Model type and text
      H.h6 $
        H.small $ do
          H.toHtml . show $ light ^. lgtModelID
          H.br
          H.toHtml . show $ light ^. lgtType
      -- Brightness widget
      H.div H.! A.class_ "progress"
            H.! A.id (H.toValue $ buildLightID lightID "brightness-container") $ do
        H.div H.! A.class_ "progress-label-container" $ do
          H.div H.! A.class_ "glyphicon glyphicon-minus minus-label" $ return ()
          H.div H.! A.class_ "glyphicon glyphicon-plus plus-label" $ return ()
          H.div H.! A.class_ "percentage-label" $
            H.small $
              H.span H.! A.id (H.toValue $ buildLightID lightID "brightness-percentage") $
                H.toHtml brightPercent
        H.div H.! A.class_ "progress-bar progress-bar-info"
              H.! A.style (H.toValue $ "width: " <> brightPercent <> ";")
              H.! A.id (H.toValue $ buildLightID lightID "brightness-bar")
              $ return ()
  addPageUIAction $ do
     -- Have light blink once after clicking the caption
     getElementByIdSafe window (buildLightID lightID "caption") >>= \caption ->
         on UI.click caption $ \_ -> do
             lightsBreatheCycle bridgeIP
                                bridgeUserID
                                [lightID]
     -- Turn on / off by clicking the light symbol
     getElementByIdSafe window (buildLightID lightID "image") >>= \image ->
         on UI.click image $ \_ -> do
             -- Query current light state to see if we need to turn it on or off
             curLights <- liftIO . atomically $ readTVar _aeLights
             case HM.lookup lightID curLights of
                 Nothing         -> return ()
                 Just lightOnOff -> do
                     lightsSwitchOnOff bridgeIP
                                       bridgeUserID
                                       [lightID]
                                       (not $ lightOnOff ^. lgtState . lsOn)
     -- Change brightness bright clicking the left / right side of the brightness bar
     --
     -- TODO: More precision (smaller increments) when controlling individual lights,
     --       maybe also make the adjustment curve non-linear (more precision at the
     --       beginning)
     --
     getElementByIdSafe window (buildLightID lightID "brightness-container") >>= \image ->
         on UI.mousedown image $ \(mx, _) -> do
             -- Construct and perform REST API call
             lightsChangeBrightness bridgeIP
                                    bridgeUserID
                                    _aeLights
                                    [lightID]
                                    -- Click on left part decrements, right part increments
                                    (if mx < 50 then (-brightnessChange) else brightnessChange)
     -- Respond to clicks on the color picker
     when colorSupport $
         getElementByIdSafe window (buildLightID lightID "color-picker-overlay") >>= \image ->
             on UI.mousedown image $ \(mx, my) -> do
                 case xyFromColorPickerCoordinates _aeColorPickerImg mx my (light ^. lgtModelID) of
                     CPR_Margin       -> return ()
                     CPR_SetColorLoop ->
                         lightsColorLoop bridgeIP
                                         bridgeUserID
                                         _aeLights
                                         [lightID]
                     CPR_Random       -> do
                         (xyX, xyY) <- liftIO getRandomXY
                         lightsSetColorXY bridgeIP
                                          bridgeUserID
                                          _aeLights
                                          [lightID]
                                          xyX
                                          xyY
                     CPR_XY xyX xyY   ->
                         lightsSetColorXY bridgeIP
                                          bridgeUserID
                                          _aeLights
                                          [lightID]
                                          xyX
                                          xyY

getRandomXY :: IO (Float, Float)
getRandomXY = do
    let rnd8Bit = getStdRandom (randomR (0, 255)) :: IO Float
    rgb <- (,,) <$> rnd8Bit <*> rnd8Bit <*> rnd8Bit
    return $ rgbToXY rgb LM_HueBulbA19

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

-- Build group switch tile for light group
addGroupSwitchTile :: GroupName -> [LightID] -> CookieUserID -> Window -> PageBuilder ()
addGroupSwitchTile groupName groupLightIDs userID window = do
  AppEnv { .. } <- ask
  -- Get relevant bridge information, assume it won't change over the lifetime of the connection
  bridgeIP     <- liftIO . atomically $ (^. pcBridgeIP    ) <$> readTVar _aePC
  bridgeUserID <- liftIO . atomically $ (^. pcBridgeUserID) <$> readTVar _aePC
  let queryAnyLightsInGroup condition =
        (liftIO . atomically $ (,) <$> readTVar _aeLights <*> readTVar _aeLightGroups)
          >>= \(lights, lightGroups) -> return $
            anyLightsInGroup groupName lightGroups lights condition
      queryGroupShown                  =
          queryUserData _aePC userID (udVisibleGroupNames . to (HS.member groupName))
  grpHasColor <- queryAnyLightsInGroup (^. lgtType . to isColorLT)
  -- Tile
  queryAnyLightsInGroup (^. lgtState . lsOn) >>= \grpOn ->
    liftIO (atomically queryGroupShown) >>= \grpShown ->
      addPageTile $
        H.div H.! A.class_ "tile"
              H.! A.style ( H.toValue $ "opacity: "
                              <> show (if grpOn then enabledOpacity else disabledOpacity)
                              <> ";"
                          )
              H.! A.id (H.toValue $ buildGroupID groupName "tile") $ do
          -- Caption and switch icon
          H.div H.! A.class_ "light-caption light-caption-group-header small"
                H.! A.id (H.toValue $ buildGroupID groupName "caption") $ do
                  void "Group"
                  H.br
                  H.toHtml (fromGroupName groupName)
          H.img H.! A.class_ "img-rounded"
                H.! A.src "static/svg/hds.svg"
                H.! A.id (H.toValue $ buildGroupID groupName "image")
          -- Only add color picker elements for lights that support colors
          when grpHasColor $
              addColorPicker (buildGroupID groupName "tile"                  )
                             (buildGroupID groupName "color-picker-container")
                             (buildGroupID groupName "color-picker-overlay"  )
          -- Group show / hide widget
          H.button H.! A.type_ "button"
                   H.! A.class_ "btn btn-sm btn-info show-hide-btn group-switch-show-hide-btn"
                   H.! A.id (H.toValue $ buildGroupID groupName "show-btn")
                   $ H.toHtml (if grpShown then grpShownCaption else grpHiddenCaption)
          -- Brightness widget
          H.div H.! A.class_ "progress"
                H.! A.id (H.toValue $ buildGroupID groupName "brightness-container") $ do
            H.div H.! A.class_ "progress-label-container" $ do
              H.div H.! A.class_ "glyphicon glyphicon-minus minus-label" $ return ()
              H.div H.! A.class_ "glyphicon glyphicon-plus plus-label"   $ return ()
            H.div H.! A.class_   "progress-bar progress-bar-info"        $ return ()
  addPageUIAction $ do
      -- Have light blink once after clicking the caption
      getElementByIdSafe window (buildGroupID groupName "caption") >>= \caption ->
          on UI.click caption $ \_ -> do
              lightsBreatheCycle bridgeIP
                                 bridgeUserID
                                 groupLightIDs
      -- Register click handler for turning group lights on / off
      getElementByIdSafe window (buildGroupID groupName "image") >>= \image ->
          on UI.click image $ \_ -> do
              -- Query current group light state to see if we need to turn group on or off
              queryAnyLightsInGroup  (^. lgtState . lsOn)>>= \grpOn ->
                  lightsSwitchOnOff bridgeIP
                                    bridgeUserID
                                    groupLightIDs
                                    (not grpOn)
      -- Register click handler for changing group brightness
      getElementByIdSafe window (buildGroupID groupName "brightness-container") >>= \image ->
          on UI.mousedown image $ \(mx, _) ->
              -- Construct and perform REST API call
              lightsChangeBrightness bridgeIP
                                     bridgeUserID
                                     _aeLights
                                     groupLightIDs
                                     -- Click on left part decrements, right part increments
                                     (if mx < 50 then (-brightnessChange) else brightnessChange)
      -- Respond to clicks on the color picker
      when grpHasColor $
          getElementByIdSafe window (buildGroupID groupName "color-picker-overlay") >>= \image ->
              on UI.mousedown image $ \(mx, my) ->
                  -- TODO: We have to specify a single light type for the color conversion,
                  --       but we potentially set many different lights. Do a custom
                  --       conversion for each color light in the group
                  case xyFromColorPickerCoordinates _aeColorPickerImg mx my LM_HueBulbA19 of
                      CPR_Margin       -> return ()
                      CPR_SetColorLoop ->
                          lightsColorLoop bridgeIP
                                          bridgeUserID
                                          _aeLights
                                          groupLightIDs
                      CPR_Random       -> do
                          -- TODO: Assign different random color to each light
                          (xyX, xyY) <- liftIO getRandomXY
                          lightsSetColorXY bridgeIP
                                           bridgeUserID
                                           _aeLights
                                           groupLightIDs
                                           xyX
                                           xyY
                      CPR_XY xyX xyY   ->
                          lightsSetColorXY bridgeIP
                                           bridgeUserID
                                           _aeLights
                                           groupLightIDs
                                           xyX
                                           xyY
      -- Show / hide group lights
      getElementByIdSafe window (buildGroupID groupName "show-btn") >>= \btn ->
          on UI.click btn $ \_ -> do
              -- Start a transaction, flip the shown state of the group by adding /
              -- removing it from the visible list and return a list of UI actions to
              -- update the UI with the changes
              uiActions <- liftIO . atomically $ do
                  pc <- readTVar _aePC
                  let grpShown = pc
                               ^. pcUserData
                                . at userID
                                . non defaultUserData
                                . udVisibleGroupNames
                                . to (HS.member groupName)
                  writeTVar _aePC
                      $  pc
                         -- Careful not to use 'non' here, would otherwise remove the
                         -- entire user when removing the last HS entry, confusing...
                      &  pcUserData . at userID . _Just . udVisibleGroupNames
                      %~ (if grpShown then HS.delete groupName else HS.insert groupName)
                  return $
                      ( if   grpShown
                        then [ void $ element btn & set UI.text grpHiddenCaption ]
                        else [ void $ element btn & set UI.text grpShownCaption  ]
                      ) <>
                      ( (flip map) groupLightIDs $ \lightID -> getElementByIdSafe
                            window (buildLightID lightID "tile") >>= \e ->
                                void $ return e & set style
                                  [ if   grpShown
                                    then ("display", "none" )
                                    else ("display", "block")
                                  ]
                      )
              sequence_ uiActions

-- Tile for controlling all lights, also displays some bridge information
--
-- TODO: Maybe add controls for dimming / changing color of all lights?
--
addAllLightsTile :: Window -> PageBuilder ()
addAllLightsTile window = do
  AppEnv { .. } <- ask
  -- Get relevant bridge information, assume it won't change over the lifetime of the connection
  bridgeIP     <- liftIO . atomically $ (^. pcBridgeIP    ) <$> readTVar _aePC
  bridgeUserID <- liftIO . atomically $ (^. pcBridgeUserID) <$> readTVar _aePC
  -- Build tile
  void $ do
    lights <- liftIO . atomically $ readTVar _aeLights
    let lgtOn = anyLightsOn lights
    addPageTile $
      H.div H.! A.class_ "tile"
            H.! A.style ( H.toValue $ "opacity: "
                            <> show (if lgtOn then enabledOpacity else disabledOpacity)
                            <> ";"
                        )
            H.! A.id (H.toValue $ buildGroupID (GroupName "all-lights") "tile") $ do
        H.div H.! A.class_ "light-caption light-caption-group-header small" 
              H.! A.style "cursor: default;"
              $ "All Lights"
        H.img H.! A.class_ "img-rounded"
              H.! A.src "static/svg/bridge_v2.svg"
              H.! A.id (H.toValue $ buildGroupID (GroupName "all-lights") "image")
        H.h6 $
          H.small $
            sequence_ $ intersperse H.br
              [ H.toHtml $ "Model " <> _aeBC ^. bcModelID
              , H.toHtml $ "IP "    <> fromIPAddress bridgeIP
              , H.toHtml $ "API v"  <> (show $ _aeBC ^. bcAPIVersion)
              , H.toHtml $ (show $ length lights) <> " Lights Connected"
              ]
  -- Register click handler for turning all lights on / off
  addPageUIAction $
      getElementByIdSafe window (buildGroupID (GroupName "all-lights") "image") >>= \image ->
          on UI.click image $ \_ -> do
              -- Query current light state to see if we need to turn everything on or off
              lights <- liftIO . atomically $ readTVar _aeLights
              -- Fire & forget REST API call in another thread
              switchAllLights bridgeIP
                              bridgeUserID
                              (not $ anyLightsOn lights)

data ColorPickerResult = CPR_Margin         -- Click on the margin
                       | CPR_SetColorLoop   -- Click on the 'Set Color Loop' button
                       | CPR_Random         -- Click on the 'Random' button
                       | CPR_XY Float Float -- Clicked on the color / color temperature parts, XY

-- Classify results from a click on the color picker image
--
-- TODO: Add support for color temperature lights
--
xyFromColorPickerCoordinates :: JP.Image JP.PixelRGB8
                             -> Int
                             -> Int
                             -> LightModel
                             -> ColorPickerResult
xyFromColorPickerCoordinates colorPickerImg mx' my' lm =
    let wdh    = JP.imageWidth  colorPickerImg
        hgt    = JP.imageHeight colorPickerImg
        margin = 10 -- There's a margin around the image on the website
        mx     = mx' - margin
        my     = my' - margin
    in  case () of
            _ | mx < 0 || my < 0 || mx >= wdh || my >= hgt ->
                    -- Outside of the image is certainly on the margin
                    CPR_Margin
              | my < 340 ->
                    -- Inside the two color panels. Look up the color
                    -- in the color picker image and convert to XY
                    let (JP.PixelRGB8 r g b) = JP.pixelAt colorPickerImg mx my
                        (xyX, xyY)           = rgbToXY ( fromIntegral r / 255
                                                       , fromIntegral g / 255
                                                       , fromIntegral b / 255
                                                       )
                                               lm
                    in  CPR_XY xyX xyY
              | my >= 350 && mx < 145 ->
                    -- On the bottom left
                    CPR_SetColorLoop
              | my >= 350 && mx >= 155 ->
                    -- On the botom right
                    CPR_Random
              | otherwise ->
                    -- Margin between the buttons
                    CPR_Margin

-- Add color picker and 'tint' button
--
-- TODO: Maybe add a brightness adjustment area to the color picker?
-- TODO: Reduce height of central element in color picker (better for smaller screens)
-- TODO: Button for alert mode next to the color loop button, would be good to have
--       scenes that have blinking lights so we can eventually trigger them from schedules
--
addColorPicker :: String -> String -> String -> H.Html
addColorPicker tileID containerID overlayID = do
  H.div H.! A.class_ "color-picker-curtain"
        H.! A.style "display: none;"
        H.! A.onclick
          -- Close after a click, but only on the curtain itself, not the picker
          ( H.toValue $
              "if (event.target.id == '" <> containerID <> "') { this.style.display = 'none' }"
          )
        H.! A.id (H.toValue containerID) $
    H.img H.! A.class_ "color-picker-overlay"
          H.! A.src "static/color_picker.png"
          H.! A.id (H.toValue overlayID)
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
                          tileID
                          containerID
                          :: String
                        )
          ) $
    H.div H.! A.class_ "glyphicon glyphicon-tint color-picker-tint-icon" $ return ()

-- Tile for shutting down / rebooting server
--
-- TODO: Add option to backup / restore configuration
-- TODO: Add a 'Log' button to see record of last ten errors / warnings
--
addServerTile :: Window -> PageBuilder ()
addServerTile window = do
  AppEnv { .. } <- ask
  -- System status
  (avg15m, ramUsage) <- liftIO getSystemStatus
  -- Build tile
  void $ do
    addPageTile $
      H.div H.! A.class_ "tile" $ do
        H.div H.! A.class_ "light-caption light-caption-group-header small"
              H.! A.style "cursor: default;"
              $ "Server"
        H.img H.! A.class_ "img-rounded"
              H.! A.src "static/svg/raspberrypi.svg"
              H.! A.style "cursor: default;"
        H.div H.! A.id "server-warning" $ do
          H.h6 $
            H.small $ do
              H.toHtml $ (printf "CPU Load: %.2f" avg15m :: String)
              H.br
              H.toHtml $ (printf "RAM Usage: %.1f%%" ramUsage :: String)
          H.button H.! A.type_ "button"
                   H.! A.class_ "btn btn-danger btn-sm"
                   H.! A.onclick "getElementById('server-danger-bttns').style.display='block';"
                   $ "Admin"
        H.div H.! A.class_ "btn-group-vertical btn-group-sm"
              H.! A.id "server-danger-bttns"
              H.! A.style "display: none;" $ do
          H.button H.! A.type_ "button"
                   H.! A.class_ "btn btn-danger"
                   H.! A.id "server-shutdown-bttn"
                   $ "Shutdown"
          H.button H.! A.type_ "button"
                   H.! A.class_ "btn btn-danger"
                   H.! A.id "server-reboot-bttn"
                   $ "Reboot"
          H.button H.! A.type_ "button"
                   H.! A.class_ "btn btn-scene"
                   H.! A.onclick "getElementById('server-danger-bttns').style.display='none';" $
                     H.span H.! A.class_ "glyphicon glyphicon-chevron-left" $ return ()
  -- Register click handler for shutdown / reboot
  addPageUIAction $
      getElementByIdSafe window "server-shutdown-bttn" >>= \bttn ->
          on UI.click bttn $ \_ -> do
              liftIO $ callCommand "sudo shutdown now"
  addPageUIAction $
      getElementByIdSafe window "server-reboot-bttn" >>= \bttn ->
          on UI.click bttn $ \_ -> do
              liftIO $ callCommand "sudo shutdown -r now"

