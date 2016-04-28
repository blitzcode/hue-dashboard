
{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Main (main) where

import Data.Monoid
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Control.Lens
import Control.Concurrent.STM
import qualified Codec.Picture as JP

import Trace
import App
import AppDefs
import HueREST
import HueSetup
import PersistConfig

main :: IO ()
main =
    -- Setup tracing (TODO: Also enable tracing into a log file?)
    withTrace Nothing True False True TLInfo $ do
        -- Load configuration (might not be there)
        let configFile = "./config.yaml" -- TODO: Maybe use ~/.hue-dashboard for this?
        mbCfg <- loadConfig configFile
        -- Bridge connection and user ID
        bridgeIP <- discoverBridgeIP    $ view pcBridgeIP <$> mbCfg
        userID   <- createUser bridgeIP $ view pcUserID   <$> mbCfg
        -- We have everything setup, build and store configuration
        let newCfg = (fromMaybe defaultPersistConfig mbCfg)
                         & pcBridgeIP .~ bridgeIP
                         & pcUserID   .~ userID
        storeConfig configFile newCfg
        -- Request full bridge configuration
        traceS TLInfo $ "Trying to obtain full bridge configuration..."
        bridgeConfig <- bridgeRequestRetryTrace MethodGET bridgeIP noBody userID "config"
        traceS TLInfo $ "Success, full bridge configuration:\n" <> show bridgeConfig
        -- Request all scenes (TODO: Maybe do this on every new connection, not once per server?)
        -- http://www.developers.meethue.com/documentation/scenes-api#41_get_all_scenes
        traceS TLInfo $ "Trying to obtain list of bridge scenes..."
        _aeScenes <- bridgeRequestRetryTrace MethodGET bridgeIP noBody userID "scenes"
        traceS TLInfo $ "Success, number of scenes received: " <> show (length _aeScenes)
        -- TVars for sharing light / group state across threads
        _aeLights      <- atomically . newTVar $ HM.empty
        _aeLightGroups <- atomically . newTVar $ HM.empty
        -- TChan for propagating light updates
        _aeBroadcast <- atomically $ newBroadcastTChan
        -- Load color picker image
        _aeColorPickerImg <- JP.readPng "static/color_picker.png" >>= \case
            Right (JP.ImageRGBA8 image) -> do traceS TLInfo $ "Loaded color picker image"
                                              return image
            Right _                     -> traceAndThrow $ "Color picker image wrong format"
            Left err                    -> traceAndThrow $ "Can't load color picker image: " <> err
        -- Launch application
        run AppEnv { _aePC = newCfg
                   , _aeBC = bridgeConfig
                   , ..
                   }

