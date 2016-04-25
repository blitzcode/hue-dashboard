
{-# LANGUAGE TemplateHaskell #-}

module AppDefs where

import Control.Lens
import Control.Monad.Reader
import Control.Concurrent.STM
import Data.Word

import PersistConfig (PersistConfig)
import HueJSON

-- Some definitions for the App module which we split out here

-- Channel with light ID and update pair
type LightUpdateTChan = TChan (String, LightUpdate)

-- Different updates to the displayed light state
data LightUpdate = LU_OnOff      !Bool
                 | LU_Brightness !Word8
                 | LU_Color      !(Float, Float, Float) -- RGB
                   deriving Show

-- Application state
data AppEnv = AppEnv
    { _aePC        :: !PersistConfig
    , _aeBC        :: !BridgeConfig
    , _aeLights    :: !(TVar Lights)
    , _aeBroadcast :: !LightUpdateTChan
    }

makeLenses ''AppEnv

-- Our main application monad
type AppT m = ReaderT AppEnv m
type AppIO = AppT IO

