
{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, ScopedTypeVariables #-}

module App ( run
           ) where

import qualified Data.HashMap.Strict as HM
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent.Async
import Text.Printf

import Util
import AppDefs
import HueJSON
import HueREST
import PersistConfig
import WebUI
import LightColor

_traceBridgeState :: AppIO ()
_traceBridgeState = do
    -- Debug print light information
    lights <- HM.elems <$> (use asLights >>= liftIO . atomically . readTVar)
    liftIO . forM_ lights $ \light -> do
        putStr $ printf "%-25s | %-20s | %-22s | %-10s | %-4.1f%% | %-3s\n"
                        (light ^. lgtName)
                        (show $ light ^. lgtType)
                        (show $ light ^. lgtModelID)
                        ( if   light ^. lgtState . lsReachable
                          then "Reachable"
                          else "Not Reachable"
                          :: String
                        )
                        ( (fromIntegral (light ^. lgtState . lsBrightness . non 255) * 100)
                          / 255 :: Float
                        )
                        (if light ^. lgtState . lsOn then "On" else "Off" :: String)
    liftIO $ putStrLn ""

-- Update our local cache of the relevant bridge state, propagate changes to all UI threads
fetchBridgeState :: AppIO ()
fetchBridgeState = do
    -- Bridge
    bridgeIP <- use $ asPC . pcBridgeIP
    userID   <- use $ asPC . pcUserID
    -- Request all light information
    (newLights :: Lights) <- bridgeRequestRetryTrace MethodGET bridgeIP noBody userID "lights"
    -- Go over all the lights
    ltvar <- use asLights
    tchan <- use asUpdate
    oldLights <- liftIO . atomically $ readTVar ltvar
    forM_ (HM.toList newLights) $ \(lightID, newLight) -> do
      case HM.lookup lightID oldLights of
        Nothing       -> return () -- TODO: New light, we don't do anything here yet
        Just oldLight -> do
          -- Compare state and broadcast changes
          let writeChannel = liftIO . atomically . writeTChan tchan
          when (oldLight ^. lgtState . lsOn /= newLight ^. lgtState . lsOn) $
              writeChannel (lightID, LU_OnOff $ newLight ^. lgtState . lsOn)
          when (oldLight ^. lgtState . lsBrightness /= newLight ^. lgtState . lsBrightness) $
              writeChannel (lightID, LU_Brightness $ newLight ^. lgtState . lsBrightness . non 255)
          when (colorFromLight oldLight /= colorFromLight newLight) $
              writeChannel (lightID, LU_Color $ colorFromLight newLight)
    -- Store new light state
    liftIO . atomically . writeTVar ltvar $ newLights

-- Application main loop, poll and update every second
mainLoop :: AppIO ()
mainLoop = do
    fetchBridgeState
    -- _traceBridgeState
    waitNSec 1
    mainLoop

-- Start up application
run :: AppState -> IO ()
run as =
    -- Web UI
    withAsync (webUIStart (as ^. asLights) (as ^. asUpdate)) $ \_ ->
        -- Application monad
        flip evalStateT as $
            mainLoop

