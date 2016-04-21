{-# LANGUAGE   OverloadedStrings
             , RecordWildCards
             , LambdaCase
             , ScopedTypeVariables #-}

module App ( run
           ) where

import Data.Aeson hiding ((.=))
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Catch
import Text.Printf

import Trace
import Util
import AppDefs
import HueREST
import PersistConfig

-- TODO: Finish this
data Light = Light { lgtName :: !String
                   , lgtType :: !String
                   , lgtOn   :: !Bool
                   } deriving Show

instance FromJSON Light where
    parseJSON j = do (Object o) <- parseJSON j
                     ls <- o .: "state"
                     Light <$> o .: "name" <*> o .: "type" <*> ls .: "on"

-- TODO: Store this in the state, maybe even have a worker thread do it
traceAllLights :: (MonadIO m, MonadCatch m) => IPAddress -> String -> m ()
traceAllLights bridgeIP userID = do
    -- Request all light information
    --
    -- http://www.developers.meethue.com/documentation/lights-api#11_get_all_lights
    --
    try (bridgeRequest MethodGET bridgeIP noBody userID "lights") >>= \case
        Left (e :: SomeException) -> do
            -- Network / IO / parsing error
            traceS TLError $ "Failed to query lights: " <> show e
        Right err@(ResponseError { .. }) -> do
            -- Error from the bridge
            traceS TLError $ "Error response querying lights: " <> show err
        Right (ResponseOK (lights :: HM.HashMap String Light)) -> do
            -- Successfully retrieved light information
            liftIO . forM_ (HM.elems lights) $ \Light { .. } -> do
                putStr $ printf "%20s (%20s) %s\n"
                                lgtName
                                lgtType
                                (if lgtOn then "On" else "Off" :: String)
            liftIO $ putStrLn ""

run :: AppState -> IO ()
run as =
    -- Setup application monad
    flip evalStateT as $ do
        -- TODO: Spit out main loop, add general error recovery
        void . forever $ do
            bridgeIP <- use $ asPC . pcBridgeIP
            userID   <- use $ asPC . pcUserID
            traceAllLights bridgeIP userID
            waitNSec 3
        return ()

