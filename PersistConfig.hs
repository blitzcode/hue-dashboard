
{-# LANGUAGE LambdaCase, TemplateHaskell, DeriveGeneric #-}

module PersistConfig ( PersistConfig(..)
                     , pcBridgeIP
                     , pcUserID
                     , defaultPersistConfig
                     , loadConfig
                     , storeConfig
                     ) where

-- Configuration data which we persist in a file

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Yaml as Y
import Data.Aeson
import Data.Monoid
import GHC.Generics

import Util
import Trace

data PersistConfig = PersistConfig
    { _pcBridgeIP :: !IPAddress
    , _pcUserID   :: !String
    } deriving (Generic, Show)

makeLenses ''PersistConfig

-- Auto-generated instances through Generic
instance FromJSON PersistConfig
instance ToJSON   PersistConfig

-- Load / store / create persistent configuration
defaultPersistConfig :: PersistConfig
defaultPersistConfig = PersistConfig "" ""

loadConfig :: MonadIO m => FilePath -> m (Maybe PersistConfig)
loadConfig fn = do
    -- Try to load persistent configuration into the state
    traceS TLInfo "Loading persistent configuration..."
    (liftIO $ Y.decodeFileEither fn) >>= \case
        Left e    -> do traceS TLError $
                         "Can't load configuration: " <> (Y.prettyPrintParseException e)
                        traceS TLInfo "Proceeding without prior configuration data"
                        return Nothing
        Right cfg -> do traceS TLInfo $ "Configuration: " <> show cfg
                        return $ Just cfg

storeConfig :: MonadIO m => FilePath -> PersistConfig -> m ()
storeConfig fn cfg = do
    traceS TLInfo "Storing persistent configuration..."
    liftIO $ Y.encodeFile fn cfg

