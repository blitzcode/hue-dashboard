
module LightColor ( colorFromLight
                  , htmlColorFromRGB
                  ) where

import Data.Maybe
import Text.Printf
import Control.Lens

import HueJSON

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

