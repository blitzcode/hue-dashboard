
{-# LANGUAGE   OverloadedStrings
             , RecordWildCards
             , DeriveGeneric
             , TemplateHaskell
             , LambdaCase
             , ScopedTypeVariables #-}

module Main where

import Network.HTTP.Client (isIpAddress)
import Network.HTTP.Simple
import Network.HostName
import System.FilePath
import Data.Aeson hiding ((.=))
import Data.Attoparsec.Text hiding (try)
import Data.Monoid
import Data.Maybe
import Data.Either.Combinators
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml as Y
import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Catch
import Control.Concurrent
import qualified Data.ByteString.Char8 as B8
import Text.Printf
import GHC.Generics

import Trace

type IPAddress = String

-- Bridge description obtained from the broker server
data Bridge = Bridge
    { brID                :: !String
    , brInternalIPAddress :: !IPAddress
    , brName              :: !(Maybe String)
    , brMacAddress        :: !(Maybe String)
    }

instance FromJSON Bridge where
    parseJSON (Object o) = do
        ip <- o .:  "internalipaddress"
        unless (isIpAddress $ B8.pack ip) $ fail "Invalid IP address"
        Bridge <$> o .:  "id"
               <*> pure ip
               <*> o .:? "name"
               <*> o .:? "macaddress"
    parseJSON _ = fail "Expected object"

instance Show Bridge where
    show Bridge { .. } = printf "Name: %s, ID: %s, IP: %s, Mac: %s"
                               (maybe "(None)" show brName)
                               (show brID)
                               (show brInternalIPAddress)
                               (maybe "(None)" show brMacAddress)

-- Discover local Hue bridges using the broker server
--
-- http://www.developers.meethue.com/documentation/hue-bridge-discovery
--
queryBrokerServer :: (MonadThrow m, MonadIO m) => m [Bridge]
queryBrokerServer = do
    let brokerServerURL = "https://www.meethue.com/api/nupnp"
    request  <- parseRequest brokerServerURL
    response <- httpJSON request
    return (getResponseBody response :: [Bridge])

-- Bridge configuration obtained from the api/config endpoint without a whitelisted user
data BridgeConfigNoWhitelist = BridgeConfigNoWhitelist
    { bcnwSWVersion  :: !String
    , bcnwAPIVersion :: !String
    , bcnwName       :: !String
    , bcnwMac        :: !String
    }

instance Show BridgeConfigNoWhitelist where
    show BridgeConfigNoWhitelist { .. } = printf "Name: %s, SW Ver: %s, API Ver: %s, Mac: %s"
                                                 (show bcnwName)
                                                 (show bcnwSWVersion)
                                                 (show bcnwAPIVersion)
                                                 (show bcnwMac)

instance FromJSON BridgeConfigNoWhitelist where
    parseJSON (Object o) =
        BridgeConfigNoWhitelist <$> o .: "swversion"
                                <*> o .: "apiversion"
                                <*> o .: "name"
                                <*> o .: "mac"
    parseJSON _ = fail "Expected object"

-- Actual bridge configuration obtainable by whitelisted user
--
-- http://www.developers.meethue.com/documentation/configuration-api#72_get_configuration
--
data BridgeConfig = BridgeConfig
    {
    }

instance FromJSON BridgeConfig where
    parseJSON (Object o) =
        return BridgeConfig
        {-
        BridgeConfig <$> o .: "swversion"
                     <*> o .: "apiversion"
                     <*> o .: "name"
                     <*> o .: "mac"
        -}
    parseJSON _ = fail "Expected object"

requestBridgeConfig :: (MonadIO m, MonadThrow m) => IPAddress -> String -> m BridgeConfig
requestBridgeConfig bridgeIP userID = bridgeRequest MethodGET bridgeIP noBody userID "config"

data BridgeRequestMethod = MethodGET | MethodPOST | MethodPUT
                           deriving (Eq, Enum)

instance Show BridgeRequestMethod where
    show MethodGET  = "GET"
    show MethodPOST = "POST"
    show MethodPUT  = "PUT"

-- Makes it a little easier to do a request without a body
noBody :: Maybe Int
noBody = Nothing

-- Call a REST API on the Hue bridge
bridgeRequest :: forall m a body. (MonadIO m, MonadThrow m, FromJSON a, ToJSON body)
              => BridgeRequestMethod
              -> IPAddress
              -> Maybe body
              -> String
              -> String
              -> m a
bridgeRequest method bridgeIP mbBody userID apiEndPoint = do
    unless (isIpAddress $ B8.pack bridgeIP) $ fail "Invalid IP address"
    request' <- parseRequest $
                    show method <> " http://" <> bridgeIP </> "api" </> userID </> apiEndPoint
    let request = case mbBody of
                      Just j  -> setRequestBodyJSON j request'
                      Nothing -> request'
    response <- httpJSON request
    return (getResponseBody response :: a)

-- We add custom constructors for the bridge errors we actually want to handle, default
-- all others to BEOther
--
-- http://www.developers.meethue.com/documentation/error-messages
--
data BridgeError = BEUnauthorizedUser
                 | BELinkButtonNotPressed
                 | BEOther !Int
                   deriving (Eq, Show)

instance Enum BridgeError where
    toEnum 1   = BEUnauthorizedUser
    toEnum 101 = BELinkButtonNotPressed
    toEnum err = BEOther err
    fromEnum BEUnauthorizedUser     = 1
    fromEnum BELinkButtonNotPressed = 101
    fromEnum (BEOther err)          = err

data BridgeResponse a = ResponseError { reType :: !BridgeError
                                      , reAddr :: !String
                                      , reDesc :: !String
                                      }
                      | ResponseOK a
                        deriving Show

-- Generic response type from the bridge. Either we get an array containing an object with
-- the 'error' key and the type / address / description fields, or we get our wanted response
instance FromJSON a => FromJSON (BridgeResponse a) where
    parseJSON j = let parseError = do [(Object o)] <- parseJSON j
                                      err <- o .: "error"
                                      ResponseError <$> (toEnum <$> err .: "type")
                                                    <*>             err .: "address"
                                                    <*>             err .: "description"
                      parseOK    = ResponseOK <$> parseJSON j
                   in parseError <|> parseOK

-- Configuration data which we persist in a file
data PersistConfig = PersistConfig
    { _pcBridgeIP :: !IPAddress
    , _pcUserID   :: !String
    } deriving (Generic, Show)

makeLenses ''PersistConfig

-- Auto-generated instances through Generic
instance FromJSON PersistConfig
instance ToJSON   PersistConfig

-- Application state
data AppState = AppState
    { _asPC :: !PersistConfig
    }

makeLenses ''AppState

-- Our main application monad
type AppT m = StateT AppState m
type AppIO = AppT IO

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

waitNSec :: MonadIO m => Int -> m ()
waitNSec sec = liftIO . threadDelay $ sec * 1000 * 1000

data APIVersion = APIVersion { avMajor :: !Int, avMinor :: !Int, avPatch :: !Int }

instance Show APIVersion where
    show APIVersion { .. } = show avMajor <> "." <> show avMinor <> "." <> show avPatch

parseAPIVersion :: String -> Maybe APIVersion
parseAPIVersion s = rightToMaybe . parseOnly parser $ T.pack s
    where parser = APIVersion <$> (decimal <* char '.') <*> (decimal <* char '.') <*> decimal

-- Verify existing bridge IP and / or discover new one
discoverBridgeIP :: (MonadIO m, MonadCatch m) => Maybe IPAddress -> m IPAddress
discoverBridgeIP bridgeIP =
    -- Do we have a bridge IP to try?
    case bridgeIP of
        Just ip -> do
            -- Verify bridge IP by querying the bridge config. This is one of the few API
            -- requests we can make without having a whitelisted user, use it to verify
            -- that our IP points to a valid Hue bridge
            traceS TLInfo $ "Trying to verify bridge IP: " <> ip
            try (bridgeRequest MethodGET ip noBody  "no-user" "config") >>= \case
                Left (e :: SomeException) -> do
                    traceS TLError $ "Bad / stale bridge IP: " <> show e
                    discoverBridgeIP Nothing
                Right (cfg :: BridgeConfigNoWhitelist) -> do
                    traceS TLInfo $ "Success, basic bridge configuration:\n" <> (show cfg)
                    -- Obtained and traced basic configuration, check API version
                    case parseAPIVersion (bcnwAPIVersion cfg) of
                        Nothing ->
                            traceS TLError $ "Failed to parse API version: " <> (bcnwAPIVersion cfg)
                        Just av ->
                            if   avMajor av >= 1 && avMinor av >= 12
                            then traceS TLInfo $ "Bridge API version OK: " <> show av
                            else traceS TLWarn $
                                     "API version lower than expected (1.12.0), " <>
                                     "please update the bridge to avoid incompatibilities: " <>
                                     show av
                    return ip
        Nothing -> do
            -- No IP, run bridge discovery
            traceS TLInfo "Running bridge discovery using broker server..."
            try queryBrokerServer >>= \case
                Left (e :: SomeException) -> do
                    traceS TLError $ "Bridge discovery failed (retry in 5s): " <> show e
                    waitNSec 5
                    discoverBridgeIP Nothing
                Right bridges ->
                    if null bridges
                        then do traceS TLError "No bridge found (retry in 5s)"
                                waitNSec 5
                                discoverBridgeIP Nothing
                        else do traceS TLInfo $
                                    printf "Found %i bridge(s), using first:\n%s"
                                           (length bridges) (show bridges)
                                -- TODO: Try all bridges till we find a working
                                --       one instead of always going for the first
                                discoverBridgeIP . Just . brInternalIPAddress . head $ bridges

data UserNameResponse = UserNameResponse !String
                        deriving Show

instance FromJSON UserNameResponse where
    parseJSON j = do [(Object o)] <- parseJSON j
                     o' <- o .: "success"
                     UserNameResponse <$> o' .: "username"

-- Verify existing user or create new one
setupUser :: (MonadIO m, MonadCatch m) => IPAddress -> Maybe String -> m String
setupUser bridgeIP userID =
    -- Do we have a user ID to try?
    case userID of
        Just uid -> do
            -- Verify user ID by querying timezone list, which requires whitelisting
            traceS TLInfo $ "Trying to verify user ID: " <> uid
            try (bridgeRequest MethodGET bridgeIP noBody uid "info/timezones") >>= \case
                Left (e :: SomeException) -> do
                    -- Network / IO / parsing error
                    traceS TLError $ "Failed to verify user ID (retry in 5s): " <> show e
                    waitNSec 5
                    setupUser bridgeIP (Just uid)
                Right err@(ResponseError { .. }) -> do
                    -- Got an error from the bridge, just create a fresh user ID
                    traceS TLError $
                        "Error response verifying user ID: " <> show err
                    setupUser bridgeIP Nothing
                Right (ResponseOK (_ :: [String])) -> do
                    -- Looks like we got our timezone list, user ID is whitelisted and verified
                    traceS TLInfo $ "User ID seems whitelisted"
                    return uid
        Nothing  -> do
            -- Create new user on the bridge
            --
            -- http://www.developers.meethue.com/documentation/configuration-api#71_create_user
            --
            host <- liftIO getHostName
            let body = -- We use our application name and the host name, as recommended
                       HM.fromList ([("devicetype", "haskell-hue#" <> host)] :: [(String, String)])
            traceS TLInfo $ "Creating new user ID: " <> show body
            try (bridgeRequest MethodPOST bridgeIP (Just body) "" "") >>= \case
                Left (e :: SomeException) -> do
                    -- Network / IO / parsing error, retry
                    traceS TLError $ "Failed to create user ID (retry in 5s): " <> show e
                    waitNSec 5
                    setupUser bridgeIP Nothing
                Right err@(ResponseError { .. }) -> do
                    -- Error from the bridge, try again and alert user if we require the
                    -- link button on the bridge to be pressed
                    traceS TLError $
                        "Error response creating user ID (retry in 5s): " <> show err
                    case reType of
                        BELinkButtonNotPressed ->
                            traceS TLWarn $
                                "Please press the link button on the bridge to proceed"
                        _ -> return ()
                    waitNSec 5
                    setupUser bridgeIP Nothing
                Right (ResponseOK (UserNameResponse uid)) -> do
                    -- We created the user, recurse to verify whitelisting
                    traceS TLInfo $ "Successfully created new user ID: " <> uid
                    setupUser bridgeIP (Just uid)

data Light = Light { lgtName :: !String
                   , lgtType :: !String
                   , lgtOn   :: !Bool
                   } deriving Show

instance FromJSON Light where
    parseJSON j = do (Object o) <- parseJSON j
                     ls <- o .: "state"
                     Light <$> o .: "name" <*> o .: "type" <*> ls .: "on"

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

main :: IO ()
main =
  -- Setup tracing
  withTrace Nothing True False True TLInfo $ do
    -- Load configuration (might not be there)
    let configFile = "./config.yaml"
    mbCfg <- loadConfig configFile
    -- Bridge connection and user ID
    bridgeIP <- discoverBridgeIP   $ view pcBridgeIP <$> mbCfg
    userID   <- setupUser bridgeIP $ view pcUserID   <$> mbCfg
    -- We have everything setup, build and store configuration
    let newCfg = (fromMaybe defaultPersistConfig mbCfg)
                     & pcBridgeIP .~ bridgeIP
                     & pcUserID   .~ userID
    storeConfig configFile newCfg
    -- Setup application monad
    flip evalStateT AppState { _asPC = newCfg
                             } $ do
      void . forever $ do
          traceAllLights bridgeIP userID
          waitNSec 3
      return ()

