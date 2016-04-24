
{-# LANGUAGE TemplateHaskell #-}

module AppDefs where

import Control.Lens
import Control.Monad.State
import Control.Concurrent.STM.TVar

import PersistConfig (PersistConfig)
import HueJSON

-- Some definitions for the App module which we split out here

-- Application state
data AppState = AppState
    { _asPC     :: !PersistConfig
    , _asBC     :: !BridgeConfig
    , _asLights :: !(TVar Lights)
    }

makeLenses ''AppState

-- Our main application monad
type AppT m = StateT AppState m
type AppIO = AppT IO

