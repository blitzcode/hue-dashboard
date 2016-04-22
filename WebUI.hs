
{-# LANGUAGE OverloadedStrings #-}

module WebUI ( webUIStart
             ) where

import Data.Monoid
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Trace
import HueJSON

-- Threepenny based user interface for inspecting and controlling Hue devices

-- TODO: This is an extremely basic, proof-of-concept implementation. There's no real time
--       update of the light status and no way to change their state

webUIStart :: MonadIO m => TVar [Light] -> m ()
webUIStart lights = do
    -- Start server
    let port = 8001
    traceS TLInfo $ "Starting web server on all interfaces, port " <> show port
    liftIO . startGUI
        defaultConfig { jsPort = Just port
                      , jsAddr = Just "0.0.0.0" -- All interfaces, not just loopback
                      , jsLog  = traceB TLInfo
                      }
        $ setup lights

setup :: TVar [Light] -> Window -> UI ()
setup lights' window = do
    void $ return window # set title "Hue Lights"

    lights <- liftIO . atomically $ readTVar lights'

    return ()

