
{-# LANGUAGE TemplateHaskell #-}

module AppDefs where

import Control.Lens
import Control.Monad.State
import Control.Concurrent.STM

import PersistConfig (PersistConfig)
import HueJSON
import WebUI

-- Some definitions for the App module which we split out here

-- Application state (TODO: Might be better to switch this to a reader)
data AppState = AppState
    { _asPC     :: !PersistConfig
    , _asBC     :: !BridgeConfig
    , _asLights :: !(TVar Lights)
    , _asUpdate :: !LightUpdateTChan
    }

makeLenses ''AppState

-- Our main application monad
type AppT m = StateT AppState m
type AppIO = AppT IO

