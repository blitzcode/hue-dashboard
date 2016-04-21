
module Main (main) where

import Data.Monoid
import Data.Maybe
import Control.Lens

import Trace
import App
import AppDefs
import HueREST
import HueSetup
import PersistConfig

main :: IO ()
main =
    -- Setup tracing
    withTrace Nothing True False True TLInfo $ do
        -- Load configuration (might not be there)
        let configFile = "./config.yaml"
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
        -- Launch application
        run AppState { _asPC = newCfg
                     , _asBC = bridgeConfig
                     }

