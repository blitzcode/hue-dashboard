
{-# LANGUAGE OverloadedStrings, RecordWildCards, RankNTypes, LambdaCase #-}

module WebUITileBuildingSchedules ( addSchedulesTile
                                  , addScheduleTile
                                  ) where

import Text.Printf
import Text.Read (readMaybe)
import qualified Data.Text as T
import Data.Monoid
import Data.Maybe
import Data.Time.Clock.POSIX (getPOSIXTime)
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
import AppDefs
import PersistConfig
import WebUIHelpers

-- Code for building the schedule tiles

-- TODO: This module has a fair bit in common with WebUITileBuildingScenes, refactor

-- We give this CSS class to all schedule tile elements we want
-- to hide / show as part of the 'Schedules' group
scheduleTilesClass :: String
scheduleTilesClass = "schedule-tiles-hide-show"

-- Overwrite or create schedule
createSchedule :: TVar PersistConfig
               -> ScheduleName
               -> SceneName
               -> Int
               -> Int
               -> [Bool]
               -> SceneAction
               -> IO ()
createSchedule tvPC scheduleName _sScene _sHour _sMinute _sDays _sAction =
    atomically $ modifyTVar' tvPC (pcSchedules . at scheduleName ?~
        Schedule { _sTrigStatus = STJustCreated, .. })

-- Weekday names
days :: [String]
days = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

-- TODO: Schedule creation and deletion currently requires a page reload

scheduleCreatorID, scheduleCreatorNameID, scheduleCreatorHourID, scheduleCreatorMinuteID,
    scheduleCreatorActionID, scheduleCreatorSceneID, actionActivate, actionTurnOff, actionBlink
    :: String
scheduleCreatorDayID :: String -> String
scheduleCreatorID        = "schedule-creator-dialog-container"
scheduleCreatorNameID    = "schedule-creator-dialog-name"
scheduleCreatorHourID    = "schedule-creator-dialog-hour"
scheduleCreatorMinuteID  = "schedule-creator-dialog-minute"
scheduleCreatorActionID  = "schedule-creator-dialog-action"
scheduleCreatorSceneID   = "schedule-creator-dialog-scene"
scheduleCreatorDayID day = "schedule-creator-dialog-day" <> day
actionActivate           = "activate"
actionTurnOff            = "turn-off"
actionBlink              = "blink"

-- Build the head tile for toggling visibility and creation of schedules. Return if the
-- 'Schedules' group is visible and subsequent elements should be added hidden or not
addSchedulesTile :: [SceneName] -> CookieUserID -> Window -> PageBuilder Bool
addSchedulesTile sceneNames userID window = do
  AppEnv { .. } <- ask
  let scheduleCreatorBtnID       = "schedule-creator-dialog-btn"        :: String
      schedulesTileHideShowBtnID = "schedules-tile-hide-show-btn"       :: String
      schedulesTileGroupName     = GroupName "<SchedulesTileGroup>"
      queryGroupShown            =
        queryUserData _aePC userID (udVisibleGroupNames . to (HS.member schedulesTileGroupName))
  grpShown <- liftIO (atomically queryGroupShown)
  -- Client and server epoch time in ms, compute difference and complain if it differs too much
  clientTime <- liftIO $ runUI window (callFunction $ ffi "(new Date()).getTime()" :: UI Double)
  serverTime <- (1000 *) . realToFrac <$> liftIO getPOSIXTime :: PageBuilder Double
  let timeDiff          = abs $ clientTime - serverTime
      timeDiffThreshold = 120 * 1000 -- 2min
  when (timeDiff > timeDiffThreshold) $
      traceS TLWarn $ printf "Time difference between client and server is %i seconds"
                      (round $ timeDiff / 1000 :: Int)
  -- Tile
  addPageTile $
    H.div H.! A.class_ "tile" $ do
      -- Caption and scene icon
      H.div H.! A.class_ "light-caption light-caption-group-header small"
            H.! A.style "cursor: default;"
            $ "Schedules"
      H.img H.! A.class_ "img-rounded"
            H.! A.src "static/svg/clock.svg"
            H.! A.style "cursor: default;"
      -- Schedule creation dialog
      H.div H.! A.class_ "color-picker-curtain"
            H.! A.style "display: none;"
            H.! A.id (H.toValue scheduleCreatorID)
            H.! A.onclick
              -- Close after a click, but only on the curtain itself, not the dialog
              ( H.toValue $
                  "if(event.target.id=='" <> scheduleCreatorID <> "'){this.style.display='none'}"
              )
            $ do
        H.div H.! A.class_ "scene-creator-frame" $ do
          H.div H.! A.class_ "small" $ do
            void $ "at "
            H.select H.! A.id (H.toValue scheduleCreatorHourID) $
              forM_ ([0..23] :: [Int]) $ \h ->
                if  h == 16 -- Default
                then H.option H.! A.value "16" H.! A.selected "selected" $ "16"
                else H.option H.! A.value (H.toValue . show $ h) $ H.toHtml (show h)
            void $ " hour "
            H.select H.! A.id (H.toValue scheduleCreatorMinuteID) $
              forM_ ([0..59] :: [Int]) $ \m ->
                if  m == 30 -- Default
                then H.option H.! A.value "30" H.! A.selected "selected" $ "30"
                else H.option H.! A.value (H.toValue . show $ m) $ H.toHtml (show m)
            void $ " minutes"
            H.br >> H.br
            void $ "do "
            H.select H.! A.id (H.toValue scheduleCreatorActionID) $ do
              H.option H.! A.value (H.toValue actionActivate) $ "activate"
              H.option H.! A.value (H.toValue actionTurnOff ) $ "turn lights off in"
              H.option H.! A.value (H.toValue actionBlink   ) $ "blink lights in"
            H.br >> H.br
            void $ "scene "
            H.select H.! A.id (H.toValue scheduleCreatorSceneID) $
              forM_ sceneNames $ \s ->
                H.option H.! A.value (H.toValue s) $ H.toHtml s
            void $ " on"
            H.br >> H.br
            H.div H.! A.class_ "schedule-day-container" $
              forM_ days $ \day ->
                H.div H.! A.class_ "day" $ do
                  H.input H.! A.type_ "checkbox"
                          H.! A.id (H.toValue $ scheduleCreatorDayID day)
                          H.! A.checked "checked"
                  H.br
                  H.toHtml day
          H.br
          H.div H.! A.class_ "input-group" $ do
            H.input H.! A.type_ "text"
                    H.! A.class_ "form-control input-sm"
                    H.! A.maxlength "30"
                    H.! A.placeholder "Name Required"
                    H.! A.id (H.toValue scheduleCreatorNameID)
            H.span H.! A.class_ "input-group-btn" $
              H.button H.! A.class_ "btn btn-sm btn-info"
                       H.! A.id (H.toValue scheduleCreatorBtnID)
                       $ "Create / Update"
          H.h6 $
            H.small $
              H.toHtml $
                  ( "Schedules allow to program reoccurring changes to the state of lights"
                    :: String
                  )
      -- Server / client time status
      H.h6 $
          if   timeDiff > timeDiffThreshold
          then H.small H.! A.style "color: red;" $ do
                 H.span H.! A.class_ "glyphicon glyphicon-remove" $ return ()
                 H.toHtml (" Server Time Differs" :: String)
          else H.small H.! A.style "color: green;" $ do
                 H.span H.! A.class_ "glyphicon glyphicon-ok" $ return ()
                 H.toHtml (" Server Time Matches" :: String)
      -- Group show / hide widget and 'New' button
      H.div H.! A.class_ "btn-group btn-group-sm" $ do
        H.button H.! A.type_ "button"
                 H.! A.class_ "btn btn-scene plus-btn"
                 H.! A.onclick
                   ( H.toValue $
                       "getElementById('" <> scheduleCreatorID <>"').style.display = 'block'"
                   ) $
                   H.span H.! A.class_ "glyphicon glyphicon-plus" $ return ()
        H.button H.! A.type_ "button"
                 H.! A.class_ "btn btn-info show-hide-btn"
                 H.! A.id (H.toValue schedulesTileHideShowBtnID)
                 $ H.toHtml (if grpShown then grpShownCaption else grpHiddenCaption)
  addPageUIAction $ do
      -- Create a new scene
      getElementByIdSafe window scheduleCreatorBtnID >>= \btn ->
          on UI.click btn $ \_ -> do
              -- Schedule name
              scheduleName <- -- Trim, autocorrect adds spaces
                              T.unpack . T.strip . T.pack <$>
                                  (get value =<< getElementByIdSafe window scheduleCreatorNameID)
              -- Scene name
              sceneName    <- get value =<< getElementByIdSafe window scheduleCreatorSceneID
              -- Hour
              hour         <- fromMaybe 16 . readMaybe <$>
                                  (get value =<< getElementByIdSafe window scheduleCreatorHourID)
              -- Minute
              minute       <- fromMaybe 30 . readMaybe <$>
                                  (get value =<< getElementByIdSafe window scheduleCreatorMinuteID)
              -- Active days
              daysActive   <- forM days $ \day ->
                  get UI.checked =<< getElementByIdSafe window (scheduleCreatorDayID day)
              -- Action
              actionStr    <- get value =<< getElementByIdSafe window scheduleCreatorActionID
              let action | actionStr == actionActivate = SAActivate
                         | actionStr == actionTurnOff  = SATurnOff
                         | actionStr == actionBlink    = SABlink
                         | otherwise                   = SAActivate
              -- Don't bother creating schedules without name
              -- TODO: Show an error message to indicate what the problem is
              -- TODO: Deal with the situation where we have no scenes at all
              unless (null scheduleName) $ do
                  liftIO $ createSchedule _aePC
                                          scheduleName
                                          sceneName
                                          hour
                                          minute
                                          daysActive
                                          action
                  traceS TLInfo $ printf
                      "Created new schedule '%s' triggering at %i:%i action '%s' scene '%s' on %s"
                      scheduleName
                      hour
                      minute
                      (show action)
                      sceneName
                      ( concatMap (\(i, active) -> if   active
                                                   then days !! i
                                                   else ""
                                  ) $ zip [0..] daysActive
                      )
                  reloadPage
      -- Show / hide schedules
      getElementByIdSafe window schedulesTileHideShowBtnID >>= \btn ->
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
                                   . to (HS.member schedulesTileGroupName)
                  writeTVar _aePC
                      $  pc
                         -- Careful not to use 'non' here, would otherwise remove the
                         -- entire user when removing the last HS entry, confusing...
                      &  pcUserData . at userID . _Just . udVisibleGroupNames
                      %~ ( if   grpShownNow
                           then HS.delete schedulesTileGroupName
                           else HS.insert schedulesTileGroupName
                         )
                  return $
                      ( if   grpShownNow
                        then [ void $ element btn & set UI.text grpHiddenCaption ]
                        else [ void $ element btn & set UI.text grpShownCaption  ]
                      ) <>
                      -- Hide or show all members of the schedule group. We do this by
                      -- identifying them by a special CSS class instead of just setting
                      -- them from names in our schedule database. This ensures we don't try
                      -- to set a non-existing element in case another users has created
                      -- a schedule not yet present in our DOM as a tile
                      ( [ getElementsByClassName window scheduleTilesClass >>= \elems ->
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

-- Add a tile for an individual schedule
addScheduleTile :: ScheduleName -> Schedule -> Bool -> Window -> PageBuilder ()
addScheduleTile scheduleName Schedule { .. } shown window = do
  AppEnv { .. } <- ask
  scenes <- _pcScenes <$> (liftIO . atomically . readTVar $ _aePC)
  let editDeleteDivID    = "schedule-" <> scheduleName <> "-edit-delete-div"
      deleteConfirmDivID = "schedule-" <> scheduleName <> "-confirm-div"
      deleteConfirmBtnID = "schedule-" <> scheduleName <> "-confirm-btn"
      minute             = show _sMinute
      minutePretty       = if length minute == 1 then "0" <> minute else minute
      sceneMissing       = not $ HM.member _sScene scenes
      opacity            = if or _sDays then enabledOpacity else disabledOpacity
  -- Tile
  addPageTile $
    H.div H.! A.class_ (H.toValue $ "tile " <> scheduleTilesClass)
          H.! A.style ( H.toValue $ "opacity: " <> show opacity <> ";" <>
                          if shown then "display: block;" else "display: none;"
                      )
          $ do
      -- Caption
      H.div H.! A.class_ "light-caption small"
            H.! A.style "cursor: default;"
            $ H.toHtml scheduleName
      -- Schedule information
      H.div H.! A.class_ "schedule-time-display" $ do
        H.span H.! A.class_ "glyphicon glyphicon-time"
               H.! A.style "vertical-align: middle;"
               $ return ()
        H.span H.! A.style "vertical-align: middle;" $
          H.toHtml $ " " <> show _sHour <> ":" <> minutePretty
      H.div $
        forM_ (zip days _sDays) $ \(dayName, dayEnabled) ->
          H.span H.! A.class_ (if dayEnabled then "day-enabled" else "day-disabled") $
            H.toHtml $ take 2 dayName <> " "
      H.span H.! A.class_ "glyphicon glyphicon-chevron-down schedule-chevron-down" $ return ()
      H.div H.! A.class_ "schedule-action" $
        case _sAction of
          SAActivate -> "Activate"
          SATurnOff  -> "Turn Off"
          SABlink    -> "Blink"
      H.div H.! A.class_ "light-caption small no-padding-margin"
            H.! A.style "cursor: default;"
            $ do
        H.toHtml _sScene
        when sceneMissing $ do
          H.toHtml (" " :: String)
          H.span H.! A.class_ "glyphicon glyphicon-alert"
                 H.! A.style "color: red;"
                 $ return ()
      -- Edit and delete button
      let editOnClick =
            -- Minutes and hours
            "getElementById('" <> scheduleCreatorMinuteID <> "').value = '" <>
              show _sMinute <> "';" <>
            "getElementById('" <> scheduleCreatorHourID <> "').value = '" <>
              show _sHour <> "';" <>
            -- Action
            "getElementById('" <> scheduleCreatorActionID <> "').value = '" <>
              ( case _sAction of
                  SAActivate -> actionActivate
                  SATurnOff  -> actionTurnOff
                  SABlink    -> actionBlink
              ) <> "';" <>
            -- Scene, only if present
            ( if   sceneMissing
              then ""
              else "getElementById('" <> scheduleCreatorSceneID <> "').value = '" <>
                     _sScene <> "';"
            ) <>
            -- Days
            ( flip concatMap (zip days _sDays) $ \(dayName, dayEnabled) ->
                "getElementById('" <> scheduleCreatorDayID dayName <> "').checked = '" <>
                  (if dayEnabled then "checked" else "") <> "';"
            ) <>
            -- Name
            "getElementById('" <> scheduleCreatorNameID <> "').value = '" <>
              scheduleName <> "';" <>
            -- Show dialog
            "getElementById('" <> scheduleCreatorID <> "').style.display = 'block';"
      addEditAndDeleteButton editDeleteDivID
                             editOnClick
                             deleteConfirmDivID
                             deleteConfirmBtnID
  addPageUIAction $ do
      -- Delete
      getElementByIdSafe window deleteConfirmBtnID >>= \btn ->
          on UI.click btn $ \_ -> do
              liftIO . atomically $ do
                  pc <- readTVar _aePC
                  writeTVar _aePC $ pc & pcSchedules . iat scheduleName #~ Nothing
              reloadPage

