
{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module BackgroundProcessing ( pcWriterThread
                            , scheduleWatcher
                            ) where

import Data.Monoid
import Data.Aeson
import Control.Monad
import Control.Monad.Writer
import Control.Lens
import Control.Concurrent.STM
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime
import qualified Data.HashMap.Strict as HM

import Util
import Trace
import PersistConfig
import WebUIREST

-- Every N seconds we wake up and see if the configuration data we want to persist has
-- been changed. If so, we write it to disk to make sure we don't lose all data in case
-- of a crash
--
-- TODO: Risk of data corruption when being interrupted while saving at our interval
--
pcWriterThread :: TVar PersistConfig -> IO ()
pcWriterThread tvPC = loop defaultPersistConfig
  where loop lastCfg = do
          waitNSec intervalSec
          currentCfg <- atomically $ readTVar tvPC
          when (currentCfg /= lastCfg) $ do
              traceS TLInfo $ "Configuration data has changed in the last " <> show intervalSec <>
                              "s, persisting to disk..."
              storeConfig configFilePath currentCfg
          loop currentCfg
        intervalSec = 900 -- 15min

-- Schedule related processing

-- This thread executes the scene activations for our schedules
scheduleWatcher :: TVar PersistConfig -> IO ()
scheduleWatcher tvPC = loop =<< (localDay . zonedTimeToLocalTime <$> getZonedTime)
  where loop startDay = do
          curTime <- zonedTimeToLocalTime <$> getZonedTime
          -- Need to reset triggers when a new day starts
          when (diffDays (localDay curTime) startDay /= 0) $ do
            traceS TLInfo "Day has changed, resetting all triggers for the new day"
            atomically . modifyTVar' tvPC $ pcSchedules . traversed . sTrigStatus %~
              (\ts -> if ts == STAlreadyTriggered then STPending else ts)
          -- Inspect all schedules, update them and assemble list of
          -- IO actions to perform, all as a single transaction
          (ioActions :: [IO ()]) <- atomically . execWriterT $ do
            pc <- lift $ readTVar tvPC
            -- Debug print schedule state, disabled
            -- tell [ print $ pc ^. pcSchedules ]
            forM_ (pc ^. pcSchedules . to HM.toList) $ \(schedName, sched) -> do
              case sched ^. sTrigStatus of
                STJustCreated ->
                  -- We just added this, either by adding it through the UI or by loading
                  -- it from the configuration, decide what to do with it
                  if   triggerPassed sched curTime
                  then -- Already to late for this schedule today, mark as triggered
                       lift . modifyTVar' tvPC $
                         pcSchedules . at schedName . _Just . sTrigStatus .~ STAlreadyTriggered
                  else -- Still happening later today, mark as pending
                       lift . modifyTVar' tvPC $
                         pcSchedules . at schedName . _Just . sTrigStatus .~ STPending
                STAlreadyTriggered ->
                  -- Has already been triggered today, nothing left to do
                  return ()
                STPending ->
                  -- Has not yet been triggered, see if we are past the trigger time
                  when (triggerPassed sched curTime) $ do
                    -- Check week day
                    let curDayIdx = (toWeekDate (localDay curTime) ^. _3) - 1;
                        days      = sched ^. sDays
                        safeIdxDays idx | idx >= length days = False
                                        | otherwise          = days !! idx
                    if safeIdxDays curDayIdx
                      then
                        -- Active day. Does the scene exist?
                        case HM.lookup (sched ^. sScene) (pc ^. pcScenes) of
                          Nothing ->
                            -- Invalid scene
                            tell
                              [ traceS TLWarn $ "Schedule '" <> schedName <>
                                                "' tried to trigger non-existent scene '"
                                                <> (sched ^. sScene) <> "'"
                              ]
                          Just scene ->
                            -- Trigger scene
                            tell
                              [ case sched ^. sAction of
                                  SAActivate ->
                                    lightsSetScene (pc ^. pcBridgeIP) (pc ^. pcBridgeUserID) scene
                                  SAActivateSlow -> -- 15s transition time
                                    lightsSetScene (pc ^. pcBridgeIP) (pc ^. pcBridgeUserID) $
                                      scene & traversed . _2 . at "transitiontime" ?~ Number 150
                                  SATurnOff ->
                                    lightsSwitchOnOff (pc ^. pcBridgeIP)
                                                      (pc ^. pcBridgeUserID)
                                                      (map fst scene)
                                                      False
                                  SABlink ->
                                    lightsBreatheCycle (pc ^. pcBridgeIP)
                                                       (pc ^. pcBridgeUserID)
                                                       (map fst scene)
                              , traceS TLInfo $ "Schedule '" <> schedName <>
                                                "' has triggered scene '" <> (sched ^. sScene)
                                                <> "'"
                              ]
                      else
                        -- Not today
                            tell
                              [ traceS TLInfo $ "Schedule '" <> schedName <>
                                                "' skipped, not active today"
                              ]
                    -- Mark as triggered
                    lift . modifyTVar' tvPC $
                      pcSchedules . at schedName . _Just . sTrigStatus .~ STAlreadyTriggered
          -- Execute all pending actions, sleep for a while and start again
          sequence_ ioActions
          waitNSec 10
          loop $ localDay curTime

-- Did we already pass the trigger time of the schedule today?
triggerPassed :: Schedule -> LocalTime -> Bool
triggerPassed sched now =
    localTimeOfDay now > timeOfDayFromSchedule sched

-- Time since midnight, useful for comparing the day time with the trigger time
timeOfDayFromSchedule :: Schedule -> TimeOfDay
timeOfDayFromSchedule Schedule { .. } =
    TimeOfDay _sHour _sMinute $ fromIntegral (0 :: Int)

