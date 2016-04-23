
{-# LANGUAGE OverloadedStrings #-}

module WebUI ( webUIStart
             ) where

import Text.Printf
import Data.Monoid
import Control.Concurrent.STM
import Control.Lens hiding ((#), set, (<.>))
import Control.Monad
import Control.Monad.IO.Class
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.FilePath

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
        defaultConfig { jsPort       = Just port
                      , jsAddr       = Just "0.0.0.0" -- All interfaces, not just loopback
                      , jsLog        = traceB TLInfo
                      , jsStatic     = Just "static"
                      , jsCustomHTML = Just "dashboard.html"
                      }
        $ setup lights

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

setup :: TVar [Light] -> Window -> UI ()
setup lights' window = do
    -- Title
    void $ return window # set title "Hue Dashboard"
    -- Lights
    lights <- liftIO . atomically $ readTVar lights'
    forM_ lights $ \light -> do
        let desc = printf "%-25s | %-20s | %-22s | %-10s | %-4.1f%% | %-3s\n"
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
        void $ getBody window #+
            [ UI.img & set UI.src (iconFromLM $ light ^. lgtModelID)
                     & set style  [ ("width", "75px")
                                  , ("float", "left")
                                  ]
            , UI.div #. "light-tile" #+ [string desc]
            , UI.div & set style [("clear", "both")]
            ]

