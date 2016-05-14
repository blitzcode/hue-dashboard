
{-# LANGUAGE OverloadedStrings, RecordWildCards, RankNTypes, LambdaCase #-}

module WebUITileBuildingScenes ( addScenesTile
                               , addSceneTile
                               , addImportedScenesTile
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

-- Code for building the scene tiles

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
                    & maybe id (\v -> at "xy"     ?~ (lsToNA v)                 ) xy
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
  -- Scene count
  numScenes <- length . _pcScenes <$> (liftIO . atomically $ readTVar _aePC)
  -- Tile
  addPageTile $
    H.div H.! A.class_ "tile" $ do
      -- Caption and scene icon
      H.div H.! A.class_ "light-caption light-caption-group-header small"
            H.! A.style "cursor: default;"
            $ "Scenes"
      H.img H.! A.class_ "img-rounded"
            H.! A.src "static/svg/tap.svg"
            H.! A.style "cursor: default;"
      -- Scene creation dialog
      H.div H.! A.class_ "color-picker-curtain"
            H.! A.style "display: none;"
            H.! A.id (H.toValue sceneCreatorID)
            H.! A.onclick
              -- Close after a click, but only on the curtain itself, not the dialog
              ( H.toValue $
                  "if (event.target.id=='" <> sceneCreatorID <> "') { this.style.display='none' }"
              )
            $ do
        H.div H.! A.class_ "scene-creator-frame" $ do
          H.div H.! A.class_ "light-checkbox-container small" $
            -- TODO: Use Bootstrap styled form elements
            -- TODO: Add explanation text, dialog title
            -- TODO: More light selection options: all, none, all on, by group, etc.
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
      -- Scene count
      H.h6 $
        H.small $
          H.toHtml $ case numScenes of
                       0 -> "No Scenes"
                       1 -> "1 Scene"
                       _ -> show numScenes <> " Scenes"
      -- Group show / hide widget and 'New' button
      H.div H.! A.class_ "btn-group btn-group-sm" $ do
        H.button H.! A.type_ "button"
                 H.! A.class_ "btn btn-scene plus-btn"
                 H.! A.onclick
                   ( H.toValue $
                       "getElementById('" <> sceneCreatorID <>"').style.display = 'block'"
                   ) $
                   H.span H.! A.class_ "glyphicon glyphicon-plus" $ return ()
        H.button H.! A.type_ "button"
                 H.! A.class_ "btn btn-info show-hide-btn"
                 H.! A.id (H.toValue scenesTileHideShowBtnID)
                 $ H.toHtml (if grpShown then grpShownCaption else grpHiddenCaption)
  addPageUIAction $ do
      -- Create a new scene
      getElementByIdSafe window sceneCreatorBtnID >>= \btn ->
          on UI.click btn $ \_ -> do
              -- Collect scene name and included lights
              sceneNameElement <- getElementByIdSafe window sceneCreatorNameID
              sceneName        <- T.unpack . T.strip . T.pack <$> -- Trim, autocorrect adds spaces
                                      get value sceneNameElement
              inclLights       <- fmap concat . forM lightNameIDSorted $ \(_, lgtID) -> do
                  let checkboxID = sceneCreatorLightCheckboxID $ fromLightID lgtID
                  checkboxElement <- getElementByIdSafe window checkboxID
                  checkboxCheck   <- get UI.checked checkboxElement
                  return $ if checkboxCheck then [lgtID] else []
              -- Don't bother creating scenes without name or lights
              -- TODO: Show an error message to indicate what the problem is
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
  let editDeleteDivID    = "scene-" <> sceneName <> "-edit-delete-div"
      deleteConfirmDivID = "scene-" <> sceneName <> "-confirm-div"
      deleteConfirmBtnID = "scene-" <> sceneName <> "-confirm-btn"
      editBtnID          = "scene-" <> sceneName <> "-edit-btn"
      circleContainerID  = "scene-" <> sceneName <> "-circle-container"
      styleCircleNoExist = "background: white; border-color: lightgrey;" :: String
  -- Get relevant bridge information, assume it won't change over the lifetime of the connection
  bridgeIP     <- liftIO . atomically $ (^. pcBridgeIP    ) <$> readTVar _aePC
  bridgeUserID <- liftIO . atomically $ (^. pcBridgeUserID) <$> readTVar _aePC
  -- Query groups for the scene group information
  groups       <- liftIO . atomically . readTVar $ _aeLightGroups
  -- Tile
  addPageTile $
    H.div H.! A.class_ (H.toValue $ "tile " <> sceneTilesClass)
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
      -- Scene light preview (TODO: Maybe use actual light icons instead of circles?)
      H.div H.! A.class_ "circle-container"
            H.! A.id (H.toValue circleContainerID) $ do
        forM_ (take 9 $ scene) $ \(_, lgSt) ->
          let col :: String
              col | HM.lookup "on" lgSt == Just (Bool False) = "background: black;"
                  | Just (Array vXY)         <- HM.lookup "xy" lgSt,
                    [Number xXY, Number yXY] <- V.toList vXY =
                      printf "background: %s;" . htmlColorFromLightState $
                        -- Build mock LightState
                        LightState True
                                   Nothing
                                   Nothing
                                   Nothing
                                   ((\(String t) -> T.unpack t) <$> HM.lookup "effect" lgSt)
                                   (Just [realToFrac xXY, realToFrac yXY])
                                   Nothing
                                   "none"
                                   Nothing
                                   True
                  | otherwise = "background: white;"
          in  H.div H.! A.class_ "circle"
                    H.! A.style (H.toValue col)
                    $ return ()
        forM_ [0..8 - length scene] $ \_ -> -- Fill remainder with grey circles
          H.div H.! A.class_ "circle"
                H.! A.style (H.toValue styleCircleNoExist)
                $ return ()
      -- List all group names affected by the scene, truncate with ellipsis if needed
      H.h6 $
        H.small $
          H.toHtml $
            let groupsTouched =
                    flip concatMap (HM.toList groups) $ \(grpName, grpLights) ->
                      if   or $ map (\(lgtID, _) -> HS.member lgtID grpLights) scene
                      then [grpName]
                      else []
                groupStr      = concat . intersperse ", " . sort $ map fromGroupName groupsTouched
                maxLength     = 20
            in  if   length groupStr > maxLength
                then take maxLength groupStr <> "…"
                else groupStr
      -- Edit and delete button
      addEditAndDeleteButton editDeleteDivID
                             editBtnID
                             deleteConfirmDivID
                             deleteConfirmBtnID
  addPageUIAction $ do
      -- Activate
      --
      -- TODO: Maybe add a rate limiter for this? Spamming the activate button for a scene
      --       with lots of lights can really overwhelm the bridge
      --
      getElementByIdSafe window circleContainerID >>= \btn ->
          on UI.click btn $ \_ ->
              lightsSetScene bridgeIP bridgeUserID scene
      -- Edit
      getElementByIdSafe window editBtnID >>= \btn ->
          on UI.click btn $ \_ -> do
              return () -- TODO: Implement
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
      topScenes = take 7 fixNames
  -- Build scenes tile
  addPageTile $
    H.div H.! A.class_ (H.toValue $ "tile " <> sceneTilesClass)
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

