
{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, ScopedTypeVariables #-}

module App ( run
           ) where

import Data.Function
import Data.List
import qualified Data.HashMap.Strict as HM
import Control.Lens
import Control.Monad
import Control.Monad.State
import Text.Printf

import Util
import AppDefs
import HueJSON
import HueREST
import PersistConfig

traceBridgeState :: AppIO ()
traceBridgeState = do
    -- Print light information
    lights <- use asLights
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
    -- Request all light information
    (lights :: AllLights) <- bridgeRequestRetryTrace MethodGET bridgeIP noBody userID "lights"
    asLights .= sortBy (compare `on` _lgtName) (HM.elems lights)
    -- TODO: Also obtain sensor data

-- Application main loop
mainLoop :: AppIO ()
mainLoop = do
    fetchBridgeState
    traceBridgeState
    waitNSec 3
    mainLoop

-- Setup application monad
run :: AppState -> IO ()
run as =
    flip evalStateT as $
        mainLoop

