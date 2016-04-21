
{-# LANGUAGE LambdaCase, ScopedTypeVariables, RecordWildCards #-}

module Main (main) where

import Data.Monoid
import Data.Maybe
import Control.Lens
import Control.Monad.State
import Control.Monad.Catch

import Util
import Trace
import App
import AppDefs
import HueJSON
import HueREST
import HueSetup
import PersistConfig

-- Obtain full bridge configuration
-- TODO: Turn this function into a general 'do request, report errors, retry' wrapper
requestBridgeConfig :: (MonadIO m, MonadCatch m) => IPAddress -> String -> m BridgeConfig
requestBridgeConfig bridgeIP userID = do
    traceS TLInfo $ "Trying to obtain full bridge configuration..."
    try (bridgeRequest MethodGET bridgeIP noBody userID "config") >>= \case
        Left (e :: SomeException) -> do
            -- Network / IO / parsing error
            traceS TLError $ "Failed to obtain bridge configuration (retry in 5s): " <> show e
            waitNSec 5
            requestBridgeConfig bridgeIP userID
        Right err@(ResponseError { .. }) -> do
            -- Got an error from the bridge
            traceS TLError $
                "Error response obtaining bridge configuration (retry in 5s): " <> show err
            waitNSec 5
            -- TODO: It makes sense to retry if we have a connection error, but in case of
            --       something like a parsing error or an access denied type response, an
            --       endless retry loop might not do anything productive
            requestBridgeConfig bridgeIP userID
        Right (ResponseOK (cfg :: BridgeConfig)) -> do
            -- Success, trace and return
            traceS TLInfo $ "Success, full bridge configuration:\n" <> show cfg
            return cfg

main :: IO ()
main =
    -- Setup tracing
    withTrace Nothing True False True TLInfo $ do
        -- Load configuration (might not be there)
        let configFile = "./config.yaml"
        mbCfg <- loadConfig configFile
        -- Bridge connection and user ID
        bridgeIP <- discoverBridgeIP   $ view pcBridgeIP <$> mbCfg
        userID   <- createUser bridgeIP $ view pcUserID   <$> mbCfg
        -- We have everything setup, build and store configuration
        let newCfg = (fromMaybe defaultPersistConfig mbCfg)
                         & pcBridgeIP .~ bridgeIP
                         & pcUserID   .~ userID
        storeConfig configFile newCfg
        -- Request full bridge configuration
        bridgeConfig <- requestBridgeConfig bridgeIP userID
        -- Launch application
        run AppState { _asPC = newCfg
                     , _asBC = bridgeConfig
                     }
  
