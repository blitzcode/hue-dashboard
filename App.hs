
{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, ScopedTypeVariables #-}

module App ( run
           ) where

import qualified Data.HashMap.Strict as HM
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Catch
import Text.Printf

import Util
import AppDefs
import HueJSON
import HueREST
import PersistConfig

-- TODO: Store this in the state, maybe even have a worker thread do it
traceAllLights :: (MonadIO m, MonadCatch m) => IPAddress -> String -> m ()
traceAllLights bridgeIP userID = do
    -- Request all light information
    (lights :: AllLights) <- bridgeRequestRetryTrace MethodGET bridgeIP noBody userID "lights"
    -- Print light information
    liftIO . forM_ (HM.elems lights) $ \light -> do
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
    --liftIO . forM_ (HM.elems lights) $ \light -> print light
    liftIO $ putStrLn ""

-- TODO: Also obtain sensor data

mainLoop :: AppIO ()
mainLoop = do
    -- Application main loop
    bridgeIP <- use $ asPC . pcBridgeIP
    userID   <- use $ asPC . pcUserID
    traceAllLights bridgeIP userID
    waitNSec 3
    mainLoop

run :: AppState -> IO ()
run as =
    -- Setup application monad
    flip evalStateT as $
        mainLoop

