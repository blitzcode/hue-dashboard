
module LightColor ( rgbFromLight
                  , rgbToXY
                  , htmlColorFromLight
                  , htmlColorFromRGB
                  ) where

import Data.Maybe
import Text.Printf
import Control.Lens

import HueJSON

-- Color conversions between RGB and the parameters the Hue API understands
--
-- http://www.developers.meethue.com/documentation/color-conversions-rgb-xy

-- Compute a normalized RGB triplet for the given light
rgbFromLight :: Light -> (Float, Float, Float)
rgbFromLight light
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

-- Compute an XY value for the given light model from an normalized RGB triplet
--
-- TODO: We currently ignore the light model parameter and rely on the bridge to
--       clamp XY values outside the color gamut for the given light
--
rgbToXY :: (Float, Float, Float) -> LightModel -> (Float, Float)
rgbToXY (r, g, b) _ =
    let -- Gamma correction
        rGamma = if r > 0.04045 then ((r + 0.055) / (1 + 0.055)) ** 2.4 else r / 12.92
        gGamma = if g > 0.04045 then ((g + 0.055) / (1 + 0.055)) ** 2.4 else g / 12.92
        bGamma = if b > 0.04045 then ((b + 0.055) / (1 + 0.055)) ** 2.4 else b / 12.92
        -- D65 wide gamut conversion
        xX = rGamma * 0.664511 + gGamma * 0.154324 + bGamma * 0.162028;
        yY = rGamma * 0.283881 + gGamma * 0.668433 + bGamma * 0.047685;
        zZ = rGamma * 0.000088 + gGamma * 0.072310 + bGamma * 0.986039;
        -- XY
        xyzSum = xX + yY + zZ
    in  if   xyzSum == 0
        then (0, 0)
        else (xX / xyzSum, yY / xyzSum)

-- Get an HTML background color string from a light. Return a colorful gradient if the
-- light is in 'colorloop' mode
htmlColorFromLight :: Light -> String
htmlColorFromLight light =
   if   (light ^. lgtState . lsEffect == Just "colorloop")
   then "linear-gradient(to bottom right, rgb(255,100,100), rgb(100,200,100), rgb(100,100,255))"
   else htmlColorFromRGB $ rgbFromLight light

-- Convert a normalized RGB triplet into an HTML color string
htmlColorFromRGB :: (Float, Float, Float) -> String
htmlColorFromRGB (r, g, b) = printf "rgb(%.0f, %.0f, %.0f)" (r * 255) (g * 255) (b * 255)

