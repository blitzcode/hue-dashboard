
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Aeson
import Data.Hashable

-- A few minor bits and pieces shared across modules

waitNSec :: MonadIO m => Int -> m ()
waitNSec sec = liftIO . threadDelay $ sec * 1000 * 1000

newtype IPAddress = IPAddress { fromIPAddress :: String }
                    deriving (Eq, Ord, Show, ToJSON, FromJSON, Hashable)

newtype BridgeUserID = BridgeUserID { fromBridgeUserID :: String }
                       deriving (Eq, Ord, Show, FromJSON, ToJSON, Hashable)

newtype CookieUserID = CookieUserID { fromCookieUserID :: String }
                       deriving (Eq, Ord, Show, FromJSON, ToJSON, Hashable)

