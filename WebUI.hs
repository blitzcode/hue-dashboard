
{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

module WebUI ( webUIStart
             ) where

import Text.Printf
import Data.Monoid
import Data.Maybe
import Control.Concurrent.STM
import Control.Lens hiding ((#), set, (<.>))
import Control.Monad
import Control.Exception
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
                      , jsLog        = traceB TLInfo -- \_ -> return ()
                      , jsStatic     = Just "static"
                      , jsCustomHTML = Just "dashboard.html"
                      }
        $ setup lights

setup :: TVar [Light] -> Window -> UI ()
setup lights' window = do
    -- Title
    void $ return window # set title "Hue Dashboard"
    -- TODO: Bootrap's JS features need jQuery, but the version included in threepenny
    --       is too old to be supported. It seems we can't use any of the JS features in
    --       Bootstrap until it is updated
    --
    -- Bootstrap JS, should be in the body
    -- void $ getBody window #+
    --     [mkElement "script" & set (attr "src") ("static/bootstrap/js/bootstrap.min.js")]
    -- Lights
    lights <- liftIO . atomically $ readTVar lights'
    void $ (getElementByIdSafe window "lights") #+
      [ UI.div #. "thumbnails" #+
          ((flip map) lights $ \light ->
            let opacity       = if light ^. lgtState ^. lsOn then "1.0" else "0.25"
                brightPercent = printf "%.0f%%"
                                  ( fromIntegral (light ^. lgtState . lsBrightness . non 255)
                                    * 100 / 255 :: Float
                                  )
                colorStr      = htmlColorFromRGB . colorFromLight $ light
            in  ( UI.div #. "thumbnail" & set style [("opacity", opacity)]
                ) #+
                [ UI.div #. "light-caption small" #+ [string $ light ^. lgtName]
                , UI.img #. "img-rounded" & set style [ ("background", colorStr)]
                                          & set UI.src (iconFromLM $ light ^. lgtModelID)
                , UI.div #. "text-center" #+
                  [ UI.h6 #+
                    [ UI.small #+
                      [ string $ (show $ light ^. lgtModelID)
                      , UI.br
                      , string $ (show $ light ^. lgtType)
                      ]
                    ]
                  ]
                , UI.div #. "small text-center" #+ [string "Brightness"]
                , UI.div #. "progress" #+
                  [ ( UI.div #. "progress-bar progress-bar-info" &
                             set style [("width", brightPercent)]
                    ) #+
                    [ UI.small #+
                      [ string brightPercent
                      ]
                    ]
                  ]
                ]
          )
      ]

-- The getElementById function return a Maybe, but actually just throws an exception if
-- the element is not found. The exception is unfortunately completely unhelpful in
-- tracking down the reason why the page couldn't be generated. UI is unfortunately also
-- not MonadCatch, so we have to make due with runUI for the call
getElementByIdSafe :: Window -> String -> UI Element
getElementByIdSafe window elementID =
    (liftIO $ try (runUI window $ getElementById window elementID)) >>= \case
        Left (e :: SomeException) -> do
            traceS TLError $ printf "getElementById for '%s' failed: %s" elementID (show e)
            liftIO . throwIO $ e
        Right Nothing -> do
            traceS TLError $ printf "getElementById for '%s' failed" elementID
            liftIO . throwIO $ userError "getElementById failure"
        Right (Just e) ->
            return e


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

-- Compute a normalized RGB triplet for the given light
--
-- http://www.developers.meethue.com/documentation/color-conversions-rgb-xy
colorFromLight :: Light -> (Float, Float, Float)
colorFromLight light
    | hasXY =
          let -- XYZ conversion
              x = xyX
              y = xyY
              z = 1 - x - y
              yY = 1
              xX = (yY / y) * x
              zZ = (yY / y) * z
              -- sRGB D65 conversion
              r_sRGB =  xX * 1.656492 - yY * 0.354851 - zZ * 0.255038;
              g_sRGB = -xX * 0.707196 + yY * 1.655397 + zZ * 0.036152;
              b_sRGB =  xX * 0.051713 - yY * 0.121364 + zZ * 1.011530;
              -- Clamp
              (rClamp, gClamp, bClamp)
                  | r_sRGB > b_sRGB && r_sRGB > g_sRGB && r_sRGB > 1 =
                        (1, g_sRGB / r_sRGB, b_sRGB / r_sRGB) -- Red is too big
                  | g_sRGB > b_sRGB && g_sRGB > r_sRGB && g_sRGB > 1 =
                        (r_sRGB / g_sRGB, 1, b_sRGB / g_sRGB) -- Green is too big
                  | b_sRGB > r_sRGB && b_sRGB > g_sRGB && b_sRGB > 1 =
                        (r_sRGB / b_sRGB, g_sRGB / b_sRGB, 1) -- Blue is too big
                  | otherwise =
                        (r_sRGB, g_sRGB, b_sRGB)
              -- Gamma correction
              rGamma | rClamp <= 0.0031308 = 12.92 * rClamp
                     | otherwise           = (1 + 0.055) * (rClamp ** (1 / 2.4)) - 0.055
              gGamma | gClamp <= 0.0031308 = 12.92 * gClamp
                     | otherwise           = (1 + 0.055) * (gClamp ** (1 / 2.4)) - 0.055
              bGamma | bClamp <= 0.0031308 = 12.92 * bClamp
                     | otherwise           = (1 + 0.055) * (bClamp ** (1 / 2.4)) - 0.055
              -- Normalize
              (r, g, b) | rGamma > bGamma && rGamma > gGamma =
                            -- Red is largest
                            if   rGamma > 1
                            then (1, gGamma / rGamma, bGamma / rGamma)
                            else (rGamma, gGamma, bGamma)
                        | gGamma > bGamma && gGamma > rGamma =
                            -- Green is largest
                            if   gGamma > 1
                            then (rGamma / gGamma, 1, bGamma / gGamma)
                            else (rGamma, gGamma, bGamma)
                        | bGamma > rGamma && bGamma > gGamma =
                            -- Blue is largest
                            if   bGamma > 1
                            then (rGamma / bGamma, gGamma / bGamma, 1)
                            else (rGamma, gGamma, bGamma)
                        | otherwise = (rGamma, gGamma, bGamma)
          in (r, g, b)
    | otherwise = (255, 255, 255) -- TODO: Handle color temperature, might need
                                  --       to check color mode field as well
  where
        xy         = light ^. lgtState . lsXY
        hasXY      = isJust xy
        [xyX, xyY] = xy ^. non [0, 0]

-- Convert a normalized RGB triplet into an HTML color string
htmlColorFromRGB :: (Float, Float, Float) -> String
htmlColorFromRGB (r, g, b) = printf "rgb(%.0f, %.0f, %.0f)" (r * 255) (g * 255) (b * 255)

