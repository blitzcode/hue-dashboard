
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module ProcFS ( getSystemStatus
              ) where

import Data.Attoparsec.Text
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as TI
import Control.Exception
import Control.Applicative

-- Obtain the 15min CPU load average and memory usage percentage from procfs
getSystemStatus :: IO (Float, Float)
getSystemStatus = do
    -- Load and parse load average
    avg15m <- either (const 0) realToFrac .
                  parseOnly (double *> space *> double *> space *> double) <$>
                      catch (TI.readFile "/proc/loadavg")
                            (\(_ :: IOException) -> return "")
    -- Load and parse memory info
    let parseName  = takeTill (== ':') <* char ':'
        parseValue = skipSpace *> decimal <* space <* string "kB" :: Parser Int
        parseLine  = ((,) <$> parseName <*> parseValue) <* endOfLine
    meminfo <- either (const HM.empty) HM.fromList .
                  parseOnly (many parseLine) <$>
                      catch (TI.readFile "/proc/meminfo")
                            (\(_ :: IOException) -> return "")
    let total        = HM.lookupDefault 1 "MemTotal" meminfo
        buffers      = HM.lookupDefault 0 "Buffers"  meminfo
        cached       = HM.lookupDefault 0 "Cached"   meminfo
        free         = HM.lookupDefault 1 "MemFree"  meminfo
        available    = free + buffers + cached 
        usagePercent = 100 - 100 * fromIntegral available / fromIntegral total
    return (avg15m, usagePercent)

