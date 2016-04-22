
{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, ScopedTypeVariables #-}

module App ( run
           ) where

import Data.Function
import Data.List
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

_traceBridgeState :: AppIO ()
_traceBridgeState = do
    -- Debug print light information
    lights <- use asLights >>= liftIO . atomically . readTVar
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
    -- liftIO . forM_ (HM.elems lights) $ \light -> print light
    liftIO $ putStrLn ""

-- Update our local cache of the relevant bridge state
fetchBridgeState :: AppIO ()
fetchBridgeState = do
    -- Bridge
    bridgeIP <- use $ asPC . pcBridgeIP
    userID   <- use $ asPC . pcUserID
    -- Request and store all light information
    (lights :: AllLights) <- bridgeRequestRetryTrace MethodGET bridgeIP noBody userID "lights"
    tvar <- use asLights
    liftIO . atomically . writeTVar tvar .
        sortBy (compare `on` _lgtName) $ HM.elems lights -- Sort by name
    -- TODO: Also obtain sensor data

-- Application main loop
mainLoop :: AppIO ()
mainLoop = do
    fetchBridgeState
    -- _traceBridgeState
    waitNSec 3
    mainLoop

-- Start up application
run :: AppState -> IO ()
run as =
    -- Web UI
    withAsync (webUIStart $ as ^. asLights) $ \_ ->
        -- Application monad
        flip evalStateT as $
            mainLoop

