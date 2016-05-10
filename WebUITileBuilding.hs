
{-# LANGUAGE OverloadedStrings, RecordWildCards, RankNTypes #-}

module WebUITileBuilding ( addLightTile
                         , addGroupSwitchTile
                         , addAllLightsTile
                         , addScenesTile
                         , addSceneTile
                         , addImportedScenesTile
                         , addServerTile
                         ) where

import Text.Printf
import qualified Data.Text as T
import Data.Monoid
import Data.List
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Function (on)
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
import Trace
import HueJSON
import LightColor
import AppDefs
import PersistConfig
import WebUIHelpers
import WebUIREST

-- Code for building the individual tiles making up our user interface

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
      colorStr      = htmlColorFromLight light
      colorSupport  = isColorLT $ light ^. lgtType
  addPageTile $
    H.div H.! A.class_ "thumbnail"
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
      H.div H.! A.class_ "text-center" $
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

-- Apply a lens getter to the user data for the passed user ID
queryUserData :: forall a. TVar PersistConfig -> CookieUserID -> Getter UserData a -> STM a
queryUserData tvPC userID g = getUserData tvPC userID <&> (^. g)
getUserData :: TVar PersistConfig -> CookieUserID -> STM UserData
getUserData tvPC userID = readTVar tvPC <&> (^. pcUserData . at userID . non defaultUserData)

-- Captions for the show / hide group button
grpShownCaption, grpHiddenCaption :: String
grpShownCaption  = "Hide ◄"
grpHiddenCaption = "Show ►"

-- Build group switch tile for current light group
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
        H.div H.! A.class_ "thumbnail"
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
          H.div H.! A.class_ "text-center" $
            H.button H.! A.type_ "button"
                     H.! A.class_ "btn btn-sm btn-info"
                     H.! A.style "margin-top: 9px; margin-bottom: -3px;"
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

-- Reload the page
reloadPage :: UI ()
reloadPage = runFunction $ ffi "window.location.reload(false);"

-- We give this CSS class to all scene tile elements we want
-- to hide / show as part of the 'Scenes' group
sceneTilesClass :: String
sceneTilesClass = "scene-tiles-hide-show"

-- Capture the relevant state of the passed light IDs and create a named scene from it
createScene :: TVar Lights -> TVar PersistConfig -> SceneName -> [LightID] -> IO ()
createScene tvLights tvPC sceneName inclLights = atomically $ do
  lights <- readTVar tvLights
  pc     <- readTVar tvPC
  writeTVar tvPC $ pc & pcScenes . at sceneName ?~ -- Overwrite or create scene
    ( flip map inclLights $ \lgtID ->
      ( lgtID
      , case HM.lookup lgtID lights of
          Nothing   -> HM.empty -- Light with that LightID doesn't exist
          Just lgt ->
            -- For lights that are off we only have to store the off state
            if not $ lgt ^. lgtState . lsOn then HM.fromList [("on", Bool False)] else
              -- On, store all relevant light state
              -- TODO: Skip color temperature for now, seems to override color
              let lsToNA = Array . V.fromList . map (Number . realToFrac)
                  bri    = lgt ^. lgtState . lsBrightness
                  effect = lgt ^. lgtState . lsEffect
                  xy     = lgt ^. lgtState . lsXY
                  -- ct     = lgt ^. lgtState . lsColorTemp
              in  HM.empty
                    &                 at "on"     ?~ Bool True
                    & maybe id (\v -> at "bri"    ?~ (Number . fromIntegral $ v)) bri
                    & maybe id (\v -> at "effect" ?~ (String $ T.pack v)        ) effect
                    & maybe id (\v -> at "xy    " ?~ (lsToNA v)                 ) xy
                    -- & maybe id (\v -> at "ct"     ?~ (Number . fromIntegral $ v)) ct
      )
    )

-- TODO: Scene creation and deletion currently requires a page reload

-- Build the head tile for toggling visibility and creation of scenes. Return if the
-- 'Scenes' group is visible and subsequent elements should be added hidden or not
addScenesTile :: CookieUserID -> Window -> PageBuilder Bool
addScenesTile userID window = do
  AppEnv { .. } <- ask
  let sceneCreatorID                    = "scene-creator-dialog-container" :: String
      sceneCreatorNameID                = "scene-creator-dialog-name"      :: String
      sceneCreatorBtnID                 = "scene-creator-dialog-btn"       :: String
      sceneCreatorLightCheckboxID lgtID = "scene-creator-dialog-check-light-" <> lgtID
      scenesTileHideShowBtnID           = "scenes-tile-hide-show-btn"      :: String
      scenesTileGroupName               = GroupName "<ScenesTileGroup>"
  let queryGroupShown =
        queryUserData _aePC userID (udVisibleGroupNames . to (HS.member scenesTileGroupName))
  grpShown <- liftIO (atomically queryGroupShown)
  -- Sorted light names with corresponding IDs for the scene creation dialog
  lightNameIDSorted <-
    return . map (\(lgtID, lgt) -> (lgt ^. lgtName, lgtID)) .
      sortBy (compare `Data.Function.on` (^. _2 . lgtName)) . HM.toList =<<
        (liftIO . atomically $ readTVar _aeLights)
  -- Tile
  addPageTile $
    H.div H.! A.class_ "thumbnail" $ do
      -- Caption and scene icon
      H.div H.! A.class_ "light-caption light-caption-group-header small"
            H.! A.style "cursor: default;"
            $ "Scenes"
      H.img H.! A.class_ "img-rounded"
            H.! A.src "static/svg/tap.svg"
            H.! A.style "cursor: default;"
      -- Scene creation dialog
      H.div H.! A.style "display: none;"
            H.! A.id (H.toValue sceneCreatorID) $ do
        H.div H.! A.class_ "color-picker-curtain"
              H.! A.onclick "this.parentNode.style.display = 'none'"
              $ return ()
        H.div H.! A.class_ "scene-creator-frame" $ do
          H.div H.! A.class_ "light-checkbox-container small" $
            forM_ lightNameIDSorted $ \(lgtNm, lgtID) -> do -- Light checkboxes
              H.input H.! A.type_ "checkbox"
                      H.! A.id (H.toValue . sceneCreatorLightCheckboxID $ fromLightID lgtID)
              H.toHtml $ " " <> lgtNm
              H.br
          H.div H.! A.class_ "scene-create-form input-group" $ do -- Name & 'Create' button
            H.input H.! A.type_ "text"
                    H.! A.class_ "form-control input-sm"
                    H.! A.maxlength "30"
                    H.! A.placeholder "Name"
                    H.! A.id (H.toValue sceneCreatorNameID)
            H.span H.! A.class_ "input-group-btn" $
              H.button H.! A.class_ "btn btn-sm btn-info"
                       H.! A.id (H.toValue sceneCreatorBtnID)
                       $ "Create"
      -- Group show / hide widget and 'New Scene' button
      H.div H.! A.class_ "text-center" $
        H.div H.! A.class_ "btn-group-vertical btn-group-sm"
              H.! A.style "margin-top: 9px;" $ do
          H.button H.! A.type_ "button"
                   H.! A.class_ "btn btn-info"
                   H.! A.id (H.toValue scenesTileHideShowBtnID)
                   $ H.toHtml (if grpShown then grpShownCaption else grpHiddenCaption)
          H.button H.! A.type_ "button"
                   H.! A.class_ "btn btn-info"
                   H.! A.onclick
                     ( H.toValue $
                         "getElementById('" <> sceneCreatorID <>"').style.display = 'block'"
                     )
                   $ "New Scene"
  addPageUIAction $ do
      -- Create a new scene
      getElementByIdSafe window sceneCreatorBtnID >>= \btn ->
          on UI.click btn $ \_ -> do
              -- Collect scene name and included lights
              sceneNameElement <- getElementByIdSafe window sceneCreatorNameID
              sceneName        <- get value sceneNameElement
              inclLights       <- fmap concat . forM lightNameIDSorted $ \(_, lgtID) -> do
                  let checkboxID = sceneCreatorLightCheckboxID $ fromLightID lgtID
                  checkboxElement <- getElementByIdSafe window checkboxID
                  checkboxCheck   <- get UI.checked checkboxElement
                  return $ if checkboxCheck then [lgtID] else []
              -- Don't bother creating scenes without name or lights
              unless (null sceneName || null inclLights) $ do
                  liftIO $ createScene _aeLights _aePC sceneName inclLights
                  traceS TLInfo $ printf "Created new scene '%s' with %i lights"
                                         sceneName (length inclLights)
                  reloadPage
      -- Show / hide scenes
      getElementByIdSafe window scenesTileHideShowBtnID >>= \btn ->
          on UI.click btn $ \_ -> do
              -- Start a transaction, flip the shown state of the group by adding /
              -- removing it from the visible list and return a list of UI actions to
              -- update the UI with the changes
              uiActions <- liftIO . atomically $ do
                  pc <- readTVar _aePC
                  let grpShownNow = pc
                                  ^. pcUserData
                                   . at userID
                                   . non defaultUserData
                                   . udVisibleGroupNames
                                   . to (HS.member scenesTileGroupName)
                  writeTVar _aePC
                      $  pc
                         -- Careful not to use 'non' here, would otherwise remove the
                         -- entire user when removing the last HS entry, confusing...
                      &  pcUserData . at userID . _Just . udVisibleGroupNames
                      %~ ( if   grpShownNow
                           then HS.delete scenesTileGroupName
                           else HS.insert scenesTileGroupName
                         )
                  return $
                      ( if   grpShownNow
                        then [ void $ element btn & set UI.text grpHiddenCaption ]
                        else [ void $ element btn & set UI.text grpShownCaption  ]
                      ) <>
                      -- Hide or show all members of the scene group. We do this by
                      -- identifying them by a special CSS class instead of just setting
                      -- them from names in our scene database. This ensures we don't try
                      -- to set a non-existing element in case another users has created
                      -- a scene not yet present in our DOM as a tile
                      ( [ getElementsByClassName window sceneTilesClass >>= \elems ->
                            forM_ elems $ \e ->
                              element e & set style
                                [ if   grpShownNow
                                  then ("display", "none" )
                                  else ("display", "block")
                                ]
                        ]
                      )
              sequence_ uiActions
  return grpShown

-- Add a tile for an individual scene
addSceneTile :: SceneName -> Scene -> Bool -> Window -> PageBuilder ()
addSceneTile sceneName scene shown window = do
  AppEnv { .. } <- ask
  let deleteConfirmDivID = "scene-" <> sceneName <> "-confirm-div"
      deleteConfirmBtnID = "scene-" <> sceneName <> "-confirm-btn"
      circleContainerID  = "scene-" <> sceneName <> "-circle-container"
  -- Get relevant bridge information, assume it won't change over the lifetime of the connection
  bridgeIP     <- liftIO . atomically $ (^. pcBridgeIP    ) <$> readTVar _aePC
  bridgeUserID <- liftIO . atomically $ (^. pcBridgeUserID) <$> readTVar _aePC
  -- Tile
  addPageTile $
    H.div H.! A.class_ (H.toValue $ "thumbnail " <> sceneTilesClass)
          H.! A.style  ( H.toValue $ ( if   shown
                                       then "display: block;"
                                       else "display: none;"
                                       :: String
                                     )
                       )
          $ do
      -- Caption
      H.div H.! A.class_ "light-caption small"
            H.! A.style "cursor: default;"
            $ H.toHtml sceneName
      -- Scene light preview
      H.div H.! A.class_ "circle-container"
            H.! A.id (H.toValue circleContainerID) $
        forM_ ([0..8] :: [Int]) $ \_ ->
          H.div H.! A.class_ "circle"
                H.! A.style "background: white; border-color: lightgrey;"
                $ return ()
      -- Light count
      H.div H.! A.class_ "text-center" $ do
        H.h6 $
          H.small $
            "0 On, 0 Off"
        -- Delete button
        H.div H.! A.id (H.toValue deleteConfirmDivID)
              H.! A.style "display: none;" $
          H.button H.! A.type_ "button"
                   H.! A.id (H.toValue deleteConfirmBtnID)
                   H.! A.class_ "btn btn-danger btn-sm"
                   $ "Confirm"
        H.button H.! A.type_ "button"
                 H.! A.class_ "btn btn-danger btn-sm"
                 H.! A.onclick ( H.toValue $ "this.style.display = 'none'; getElementById('"
                                             <> deleteConfirmDivID <> "').style.display = 'block';"
                               )
                 $ "Delete"
  addPageUIAction $ do
      -- Activate
      getElementByIdSafe window circleContainerID >>= \btn ->
          on UI.click btn $ \_ ->
              lightsSetScene bridgeIP bridgeUserID scene
      -- Delete
      getElementByIdSafe window deleteConfirmBtnID >>= \btn ->
          on UI.click btn $ \_ -> do
              liftIO . atomically $ do
                  pc <- readTVar _aePC
                  writeTVar _aePC $ pc & pcScenes . iat sceneName #~ Nothing
              reloadPage

addImportedScenesTile :: Bool -> Window -> PageBuilder ()
addImportedScenesTile shown window = do
  AppEnv { .. } <- ask
  -- Get relevant bridge information, assume it won't change over the lifetime of the connection
  bridgeIP     <- liftIO . atomically $ (^. pcBridgeIP    ) <$> readTVar _aePC
  bridgeUserID <- liftIO . atomically $ (^. pcBridgeUserID) <$> readTVar _aePC
  let sceneBttnID sceneID = -- TODO: Move this logic to where the scenes are fetched
        -- DOM ID from scene ID
        "bridge-scene-activate-bttn-" <> fromBridgeSceneID sceneID
      nameKeyedScenes =
          -- Use the scene name as the key instead of the scene ID
          map (\(sceneID, scene) -> (scene ^. bscName, (sceneID, scene)))
              $ HM.toList _aeBridgeScenes
      nubScenes =
          -- Build 'name -> (sceneID, scene)' hashmap, resolve name
          -- collisions with the last update date
          flip HM.fromListWith nameKeyedScenes $ \sceneA sceneB ->
              case (compare `Data.Function.on` (^. _2 . bscLastUpdated)) sceneA sceneB of
                  EQ -> sceneA
                  LT -> sceneB
                  GT -> sceneA
      recentScenes =
          -- List of scenes sorted by last update date
          reverse . sortBy (compare `Data.Function.on` (^. _2 . bscLastUpdated)) .
              map snd $ HM.toList nubScenes
      fixNames =
          -- Scene names are truncated and decorated when stored on the bridge,
          -- salvage what we can and extract the cleanest UI label for them
          recentScenes & traversed . _2 . bscName %~ \sceneName ->
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
    H.div H.! A.class_ (H.toValue $ "thumbnail " <> sceneTilesClass)
          H.! A.style  ( H.toValue $ ( if   shown
                                       then "display: block;"
                                       else "display: none;"
                                       :: String
                                     )
                       )
          $ do
      H.div H.! A.class_ "light-caption small" $ do
        void $ "Imported"
        H.br
        void $ "Scenes"
      H.div H.! A.class_ "btn-group-vertical btn-group-xs scene-btn-group" $
        forM_ topScenes $ \(sceneID, scene) ->
          H.button H.! A.class_ "btn btn-scene"
                   H.! A.id (H.toValue $ sceneBttnID sceneID) $
                     H.small $ (H.toHtml $ scene ^. bscName)
  -- Register click handlers for activating the scenes
  addPageUIAction $
      forM_ topScenes $ \(sceneID, _) ->
          getElementByIdSafe window (sceneBttnID sceneID) >>= \bttn ->
              on UI.click bttn $ \_ ->
                  recallScene bridgeIP
                              bridgeUserID
                              sceneID


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
      H.div H.! A.class_ "thumbnail"
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
        H.div H.! A.class_ "text-center" $
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
--
addColorPicker :: String -> String -> String -> H.Html
addColorPicker tileID containerID overlayID = do
  H.div H.! A.style "display: none;"
        H.! A.id (H.toValue containerID) $ do
    H.div H.! A.class_ "color-picker-curtain"
          H.! A.onclick "this.parentNode.style.display = 'none'"
          $ return ()
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
addServerTile :: Window -> PageBuilder ()
addServerTile window = do
  AppEnv { .. } <- ask
  -- Build tile
  void $ do
    addPageTile $
      H.div H.! A.class_ "thumbnail" $ do
        H.div H.! A.class_ "light-caption light-caption-group-header small"
              H.! A.style "cursor: default;"
              $ "Server"
        H.img H.! A.class_ "img-rounded"
              H.! A.src "static/svg/raspberrypi.svg"
              H.! A.style "cursor: default;"
        H.div H.! A.class_ "text-center" $ do
          H.div H.! A.id "server-warning" $ do
            H.h6 $
              H.small $
                "Administrative Options"
            H.button H.! A.type_ "button"
                     H.! A.class_ "btn btn-danger btn-sm"
                     H.! A.onclick ( "getElementById('server-warning').style.display='none';" <>
                                     "getElementById('server-danger-bttns').style.display='block';"
                                   )
                     $ "Show"
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
  -- Register click handler for shutdown / reboot
  addPageUIAction $
      getElementByIdSafe window "server-shutdown-bttn" >>= \bttn ->
          on UI.click bttn $ \_ -> do
              liftIO $ callCommand "sudo shutdown now"
  addPageUIAction $
      getElementByIdSafe window "server-reboot-bttn" >>= \bttn ->
          on UI.click bttn $ \_ -> do
              liftIO $ callCommand "sudo shutdown -r now"

