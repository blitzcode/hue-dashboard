
{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Main (main) where

import Data.Monoid
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Control.Lens
import Control.Concurrent.STM
import qualified Codec.Picture as JP
import Text.Read

import Trace
import App
import AppDefs
import HueREST
import HueSetup
import PersistConfig
import CmdLineOptions

main :: IO ()
main = do
    -- Command line options
    flags <- parseCmdLineOpt
    -- Setup tracing
    let traceFn  = foldr (\f r -> case f of FlagTraceFile fn -> Just fn; _ -> r) Nothing flags
        mkTrcOpt = \case "n" -> TLNone; "e" -> TLError; "w" -> TLWarn; "i" -> TLInfo; _ -> TLInfo
        traceLvl = foldr (\f r -> case f of (FlagTraceLevel lvl) -> mkTrcOpt lvl; _ -> r)
                         TLInfo flags
    withTrace traceFn
              (not $ FlagTraceNoEcho       `elem` flags)
              (      FlagTraceAppend       `elem` flags)
              (not $ FlagTraceDisableColor `elem` flags)
              traceLvl
              $ do
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
            Right (JP.ImageRGB8 image) -> do traceS TLInfo $ "Loaded color picker image"
                                             return image
            Right _                    -> traceAndThrow $ "Color picker image wrong format"
            Left err                   -> traceAndThrow $ "Can't load color picker image: " <> err
        -- Command line options passed on to the rest of the program
        let _aeCmdLineOpts = CmdLineOpts
                { _cloPort          =
                      foldr (\f r -> case f of
                               FlagPort port -> fromMaybe defPort $ readMaybe port; _ -> r)
                            defPort
                            flags
                , _cloOnlyLocalhost = FlagLocalhost `elem` flags
                , _cloPollInterval  =
                       foldr (\f r -> case f of
                               FlagPollInterval interval ->
                                 fromMaybe defPollInterval $ readMaybe interval;
                               _ -> r)
                             defPollInterval
                             flags
                , _cloTraceHTTP     =  FlagTraceHTTP `elem` flags
                }
        -- Launch application
        run AppEnv { _aePC = newCfg
                   , _aeBC = bridgeConfig
                   , ..
                   }

