
{-# LANGUAGE TemplateHaskell #-}

module AppDefs where

import Control.Lens
import Control.Monad.Reader
import Control.Concurrent.STM
import Data.Word
import qualified Codec.Picture as JP

import PersistConfig (PersistConfig)
import HueJSON

-- Some definitions for the App module which we split out here

-- Channel with light ID and update pair
type LightUpdateTChan = TChan (String, LightUpdate)

-- Different updates to the displayed light state
data LightUpdate = LU_OnOff        !Bool
                 | LU_Brightness   !Word8
                 | LU_Color        !String -- HTML color string
                 | LU_GroupLastOff !String
                 | LU_GroupFirstOn !String
                 | LU_LastOff
                 | LU_FirstOn
                   deriving Show

-- Application state
data AppEnv = AppEnv
    { _aePC             :: !(TVar PersistConfig)
    , _aeBC             :: !BridgeConfig
    , _aeLights         :: !(TVar Lights)
    , _aeLightGroups    :: !(TVar LightGroups)
    , _aeScenes         :: !Scenes
    , _aeBroadcast      :: !LightUpdateTChan
    , _aeColorPickerImg :: !(JP.Image JP.PixelRGB8)
    , _aeCmdLineOpts    :: !CmdLineOpts
    }

data CmdLineOpts = CmdLineOpts
    { _cloPort          :: !Int
    , _cloOnlyLocalhost :: !Bool
    , _cloPollInterval  :: !Int
    , _cloTraceHTTP     :: !Bool
    }

makeLenses ''AppEnv
makeLenses ''CmdLineOpts

-- Our main application monad
type AppT m = ReaderT AppEnv m
type AppIO = AppT IO

