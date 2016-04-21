
module Util where

import Control.Concurrent
import Control.Monad.IO.Class

-- A few minor bits and pieces shared across modules

waitNSec :: MonadIO m => Int -> m ()
waitNSec sec = liftIO . threadDelay $ sec * 1000 * 1000

type IPAddress = String

