
module WebUIREST ( lightsSetState
                 , lightsSwitchOnOff
                 , lightsChangeBrightness
                 , lightsSetColorXY
                 , recallScene
                 , switchAllLights
                 ) where

import Data.Aeson (ToJSON)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Lens hiding ((<.>))
import Control.Monad
import Control.Monad.Reader
import System.FilePath

import Util
import HueJSON
import HueREST

-- Make a REST- API call in another thread to change the state on the bridge. The calls
-- are fire & forget, we don't retry in case of an error

-- http://www.developers.meethue.com/documentation/lights-api#16_set_light_state

lightsSetState :: (MonadIO m, ToJSON body) => IPAddress -> String -> [String] -> body -> m ()
lightsSetState bridgeIP userID lightIDs body =
    void . liftIO . async $
        forM_ lightIDs $ \lightID ->
            bridgeRequestTrace
                MethodPUT
                bridgeIP
                (Just body)
                userID
                ("lights" </> lightID </> "state")

lightsSwitchOnOff :: MonadIO m => IPAddress -> String -> [String] -> Bool -> m ()
lightsSwitchOnOff bridgeIP userID lightIDs onOff =
    lightsSetState bridgeIP userID lightIDs $ HM.fromList [("on" :: String, onOff)]

lightsChangeBrightness :: MonadIO m
                       => IPAddress
                       -> String
                       -> TVar Lights
                       -> [String]
                       -> Int
                       -> m ()
lightsChangeBrightness bridgeIP userID lights' lightIDs change = do
    -- First check which of the lights we got are turned on. Changing the brightness
    -- of a light in the off state will just result in an error response
    lights <- liftIO . atomically $ readTVar lights'
    let onLightIDs = filter (maybe False (^. lgtState . lsOn) . flip HM.lookup lights) lightIDs
    lightsSetState bridgeIP userID onLightIDs $ HM.fromList [("bri_inc" :: String, change)]

lightsSetColorXY :: MonadIO m
                 => IPAddress
                 -> String
                 -> TVar Lights
                 -> [String]
                 -> Float
                 -> Float
                 -> m ()
lightsSetColorXY bridgeIP userID lights' lightIDs xyX xyY = do
    -- Can only change the color of lights which are turned on and support this feature
    lights <- liftIO . atomically $ readTVar lights'
    let onAndCol l  = (l ^. lgtState . lsOn) && (l ^. lgtType . to isColorLT)
    let onAndColIDs = filter (maybe False onAndCol . flip HM.lookup lights) lightIDs
    lightsSetState bridgeIP userID onAndColIDs $ HM.fromList [("xy" :: String, [xyX, xyY])]

-- http://www.developers.meethue.com/documentation/groups-api#253_body_example

recallScene :: MonadIO m => IPAddress -> String -> String -> m ()
recallScene bridgeIP userID sceneID =
    void . liftIO . async $
        bridgeRequestTrace
            MethodPUT
            bridgeIP
            (Just $ HM.fromList [("scene" :: String, sceneID)])
            userID
            ("groups/0/action")

-- http://www.developers.meethue.com/documentation/groups-api#25_set_group_state

switchAllLights :: MonadIO m => IPAddress -> String -> Bool -> m ()
switchAllLights bridgeIP userID onOff =
    let body = HM.fromList[("on" :: String, onOff)]
    in  void . liftIO . async $
            bridgeRequestTrace
                MethodPUT
                bridgeIP
                (Just body)
                userID
                ("groups" </> "0" </> "action") -- Special group 0, all lights

