
{-# LANGUAGE   OverloadedStrings
             , RecordWildCards
             , LambdaCase
             , ScopedTypeVariables
             , TupleSections #-}

module App ( run
           ) where

import qualified Data.HashMap.Strict as HM
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.List
import Data.Function
import Text.Printf

import Util
import AppDefs
import HueJSON
import HueREST
import PersistConfig
import WebUI
import LightColor

_traceBridgeState :: AppIO ()
_traceBridgeState = do
    -- Debug print light information
    lights <- HM.elems <$> (view aeLights >>= liftIO . atomically . readTVar)
    liftIO . forM_ lights $ \light -> do
        putStr $ printf "%-25s | %-20s | %-22s | %-10s | %-4.1f%% | %-3s\n"
                        (light ^. lgtName)
                        (show $ light ^. lgtType)
                        (show $ light ^. lgtModelID)
                        ( if   light ^. lgtState . lsReachable
                          then "Reachable"
                          else "Not Reachable"
                          :: String
                        )
                        ( (fromIntegral (light ^. lgtState . lsBrightness . non 255) * 100)
                          / 255 :: Float
                        )
                        (if light ^. lgtState . lsOn then "On" else "Off" :: String)
    liftIO $ putStrLn ""

-- Build light groups from name prefixes
buildLightGroups :: Lights -> LightGroups
buildLightGroups lights =
    let lightIDAndName' = -- Build (light ID, light name) list
                          HM.toList lights & traversed . _2 %~ (^. lgtName)
        lightIDAndName  = -- Sort by name
                          sortBy (compare `Data.Function.on` snd) lightIDAndName'
        groupByPrefix   = -- Group by first word of the name, giving [[(light ID, light name)]]
                          flip groupBy lightIDAndName $ \(_, nameA) (_, nameB) ->
                              case (words nameA, words nameB) of
                                  (prefixA:_, prefixB:_) -> prefixA == prefixB
                                  _                      -> False
        lightGroups     = -- Build 'LightGroups' hashmap
                          HM.fromList . flip map groupByPrefix $ \lightGroup ->
                              case lightGroup of
                                  []          -> ("<NoGroup>", [])
                                  (_, name):_ ->
                                      ( -- Extract prefix from first light
                                        case words name of
                                            prefix:_ -> prefix
                                            _        -> "<NoName>"
                                      , -- Extract list of light IDs
                                        map fst lightGroup
                                      )
    in lightGroups

-- Update our local cache of the relevant bridge state, propagate changes to all UI threads
fetchBridgeState :: AppIO ()
fetchBridgeState = do
  -- Bridge
  bridgeIP <- view $ aePC . pcBridgeIP
  userID   <- view $ aePC . pcUserID
  -- Request all light information
  (newLights :: Lights) <- bridgeRequestRetryTrace MethodGET bridgeIP noBody userID "lights"
  -- Do all updating as a single transaction
  broadcast  <- view aeBroadcast
  tvarLights <- view aeLights
  tvarGroups <- view aeLightGroups
  liftIO . atomically $ do
    -- Fetch old state, store new one
    oldLights  <- readTVar tvarLights
    _oldGroups <- readTVar tvarGroups
    writeTVar tvarLights $ newLights
    let newGroups = buildLightGroups newLights
    writeTVar tvarGroups $ newGroups
    -- Find all changes in the light state
    forM_ (HM.toList newLights) $ \(lightID, newLight) -> do
      case HM.lookup lightID oldLights of
        Nothing       -> return () -- TODO: New light, we don't do anything here yet
        Just oldLight -> do
          -- Compare state and broadcast changes
          let writeChannel = writeTChan broadcast . (lightID, )
          when (oldLight ^. lgtState . lsOn /= newLight ^. lgtState . lsOn) $
              writeChannel . LU_OnOff $ newLight ^. lgtState . lsOn
          when (oldLight ^. lgtState . lsBrightness /= newLight ^. lgtState . lsBrightness) $
              writeChannel . LU_Brightness $ newLight ^. lgtState . lsBrightness . non 255
          when (colorFromLight oldLight /= colorFromLight newLight) $
              writeChannel . LU_Color $ colorFromLight newLight
    -- Did we turn the last light off or the first light on?
    let numLightsOn = length . filter (^. _2 . lgtState . lsOn) . HM.toList
    when (numLightsOn oldLights > 0 && numLightsOn newLights == 0) $
        writeTChan broadcast ("all-lights", LU_LastOff)
    when (numLightsOn oldLights == 0 && numLightsOn newLights > 0) $
        writeTChan broadcast ("all-lights", LU_FirstOn)

-- Application main loop, poll and update every second
mainLoop :: AppIO ()
mainLoop = do
    fetchBridgeState
    -- _traceBridgeState
    waitNSec 1
    mainLoop

-- Start up application
run :: AppEnv -> IO ()
run ae =
    -- Web UI
    withAsync (webUIStart ae) $ \_ ->
        -- Application monad
        flip runReaderT ae $
            mainLoop

