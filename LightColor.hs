
{-# LANGUAGE ScopedTypeVariables #-}

module LightColor ( rgbFromLightState
                  , rgbToXY
                  , htmlColorFromLightState
                  , htmlColorFromRGB
                  ) where

import Text.Printf
import Control.Lens
import Data.Word

import HueJSON

-- Color conversions between RGB/XY/HS/CT for the Hue API
--
-- http://www.developers.meethue.com/documentation/color-conversions-rgb-xy
-- http://www.developers.meethue.com/documentation/core-concepts#color_gets_more_complicated
-- http://www.developers.meethue.com/documentation/lights-api#14_get_light_attributes_and_state
-- http://hackage.haskell.org/package/colour
-- https://www.npmjs.com/package/color-temperature
-- https://github.com/neilbartlett/color-temperature/blob/master/index.js

-- Compute a normalized RGB triplet for the given light state
rgbFromLightState :: LightState -> (Float, Float, Float)
rgbFromLightState ls
    |  ls ^. lsColorMode == Just CMXY
    || ls ^. lsColorMode == Nothing
    ,  Just [xyX, xyY] <- ls ^. lsXY =
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
    |  ls ^. lsColorMode == Just CMHS
    || ls ^. lsColorMode == Nothing
    ,  Just hue <- ls ^. lsHue
    ,  Just sat <- ls ^. lsSaturation =
        hsToRGB hue sat
    |  ls ^. lsColorMode == Just CMCT
    || ls ^. lsColorMode == Nothing
    ,  Just ct <- ls ^. lsColorTemp =
        ctToRGB . mirecToKelvin $ fromIntegral ct
    | otherwise =
        (255, 255, 255) -- Has no color, assume white light

mirecToKelvin :: Float -> Float
mirecToKelvin m = 1000000 / m

kelvinToMirec :: Float -> Float
kelvinToMirec k = mirecToKelvin k

-- Hue and saturation are in the 0 - 65535 and 0 - 254 range
hsToRGB :: Word16 -> Word8 -> (Float, Float, Float)
hsToRGB hue sat =
    -- HSV conversion
    let h = (fromIntegral hue / 65535) * 360
        s = fromIntegral sat / 254
        v = 1
        hi = floor (h / 60) `mod` 6 :: Int
        f = mod1 (h / 60)
        p = v * (1 - s)
        q = v * (1 - f * s)
        t = v * (1 - (1 - f) * s)
    in  (\(r, g, b) -> (r * 255, g * 255, b * 255)) $ case hi of
            0 -> (v, t, p)
            1 -> (q, v, p)
            2 -> (p, v, t)
            3 -> (p, q, v)
            4 -> (t, p, v)
            5 -> (v, p, q)
            _ -> (1, 1, 1)
    -- HSL conversion
    -- let hk = fromIntegral hue / 65535
    --     s = fromIntegral sat / 254
    --     l = 0.5
    --     tr = mod1 (hk + 1 / 3)
    --     tg = mod1 hk
    --     tb = mod1 (hk - 1 / 3)
    --     q | l < 0.5 = l * (1 + s)
    --       | otherwise = l + s - l * s
    --     p = 2 * l - q
    --     component t | t < 1 / 6 = p + ((q - p) * 6 * t)
    --                 | t < 1 / 2 = q
    --                 | t < 2 / 3 = p + ((q - p) * 6 * (2 / 3 - t))
    --                 | otherwise = p
    --     mod1 x | pf < 0 = pf + 1
    --            | otherwise = pf
    --       where
    --         (_ :: Int, pf) = properFraction x
    -- in  ((component tr) * 255, (component tg) * 255, (component tb) * 255)

mod1 :: RealFrac a => a -> a
mod1 x | pf < 0 = pf + 1
       | otherwise = pf
  where
    (_ :: Int, pf) = properFraction x

ctToRGB :: Float -> (Float, Float, Float)
ctToRGB kelvin =
    let temperature = kelvin / 100;
        red | temperature < 66 = 255
            | otherwise        =
                  let red' = temperature - 55
                  in 351.97690566805693+ 0.114206453784165 * red' - 40.25366309332127 * log red'

        green | temperature < 66 =
                    let green' = temperature - 2
                    in  -155.25485562709179 - 0.44596950469579133 * green' +
                            104.49216199393888 * log green'
              | otherwise        =
                    let green' = temperature - 50
                    in  325.4494125711974 + 0.07943456536662342 * green' -
                            28.0852963507957 * log green'
        blue | temperature >= 66 = 255
             | temperature <= 20 = 0
             | otherwise         =
                   let blue' = temperature - 10
                   in  -254.76935184120902 + 0.8274096064007395 * blue' +
                           115.67994401066147 * log blue'
        clamp x = max 0 $ min 255 x
   in  (clamp red, clamp green, clamp blue)

-- Compute an XY value for the given light model from a normalized RGB triplet
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

-- Get an HTML background color string from a light state. Return a colorful gradient
-- if the light is in 'colorloop' mode
htmlColorFromLightState :: LightState -> String
htmlColorFromLightState ls =
   if   (ls ^. lsEffect == Just "colorloop")
   then "linear-gradient(to bottom right, rgb(255,100,100), rgb(100,200,100), rgb(100,100,255))"
   else htmlColorFromRGB $ rgbFromLightState ls

-- Convert a normalized RGB triplet into an HTML color string
htmlColorFromRGB :: (Float, Float, Float) -> String
htmlColorFromRGB (r, g, b) = printf "rgb(%.0f, %.0f, %.0f)" (r * 255) (g * 255) (b * 255)

