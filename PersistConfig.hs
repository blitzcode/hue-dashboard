
{-# LANGUAGE LambdaCase, TemplateHaskell, RecordWildCards, OverloadedStrings #-}

module PersistConfig ( configFilePath
                     , PersistConfig(..)
                     , UserData(..)
                     , defaultUserData
                     , pcBridgeIP
                     , pcBridgeUserID
                     , pcUserData
                     , udVisibleGroupNames
                     , defaultPersistConfig
                     , loadConfig
                     , storeConfig
                     ) where

-- Configuration and user data which we persist in a file

import Control.Monad
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import qualified Data.Yaml as Y
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Monoid

import Util
import Trace

configFilePath :: FilePath
configFilePath = "./config.yaml" -- TODO: Maybe use ~/.hue-dashboard for this?

data PersistConfig = PersistConfig
    { _pcBridgeIP     :: !IPAddress                    -- IP address of the bridge
    , _pcBridgeUserID :: !String                       -- Hue bridge user ID for
    , _pcUserData     :: !(HM.HashMap String UserData) -- User ID cookie to user data
    } deriving (Show, Eq)

defaultPersistConfig :: PersistConfig
defaultPersistConfig = PersistConfig "" "" HM.empty

instance FromJSON PersistConfig where
    parseJSON (Object o) =
      PersistConfig <$> o .:? "_pcBridgeIP" .!= _pcBridgeIP     defaultPersistConfig
                    <*> o .:? "_pcUserID"   .!= _pcBridgeUserID defaultPersistConfig
                    <*> o .:? "_pcUserData" .!= _pcUserData     defaultPersistConfig
    parseJSON _ = mzero

instance ToJSON PersistConfig where
   toJSON PersistConfig { .. }  =
       object [ "_pcBridgeIP" .= _pcBridgeIP
              , "_pcUserID"   .= _pcBridgeUserID
              , "_pcUserData" .= _pcUserData
              ]

data UserData = UserData
    { _udVisibleGroupNames :: !(HS.HashSet String) -- Groups which are not hidden / collapsed
    } deriving (Show, Eq)

defaultUserData :: UserData
defaultUserData = UserData HS.empty

instance FromJSON UserData where
    parseJSON (Object o) =
        UserData <$> o .:? "_udVisibleGroupNames" .!= _udVisibleGroupNames defaultUserData
    parseJSON _ = mzero

instance ToJSON UserData where
   toJSON UserData { .. }  =
      object [ "_udVisibleGroupNames" .= _udVisibleGroupNames
             ]

makeLenses ''UserData
makeLenses ''PersistConfig

-- Load / store / create persistent configuration

loadConfig :: MonadIO m => FilePath -> m (Maybe PersistConfig)
loadConfig fn = do
    -- Try to load persistent configuration into the state
    traceS TLInfo $ "Loading persistent configuration from'" <> fn <> "'"
    (liftIO $ Y.decodeFileEither fn) >>= \case
        Left e    -> do traceS TLError $
                         "Can't load configuration: " <> (Y.prettyPrintParseException e)
                        traceS TLInfo "Proceeding without prior configuration data"
                        return Nothing
        Right cfg -> do traceS TLInfo $ "Configuration: " <> show cfg
                        return $ Just cfg

storeConfig :: MonadIO m => FilePath -> PersistConfig -> m ()
storeConfig fn cfg =
    liftIO $ Y.encodeFile fn cfg

