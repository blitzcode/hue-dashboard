
{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts, RankNTypes #-}

module WebUIHelpers where

import Data.Monoid
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.STM
import Graphics.UI.Threepenny.Core
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Util
import HueJSON
import PersistConfig
import AppDefs (AppEnv)

-- Some utility functions split out from the WebUI / WebUITileBuilding modules

-- We build our page in a monad stack that provides the application environment and a
-- place to store all the event handlers and HTML elements that comprise it

data Page = Page { _pgTiles     :: ![H.Html]  -- Functions to generate all the tiles in the page
                 , _pgUIActions :: ![UI ()]   -- Functions to register all event handlers etc.
                                              --   once the page has been build
                 }

makeLenses ''Page

type PageBuilder = StateT Page (ReaderT AppEnv IO)

addPageTile :: MonadState Page m => H.Html -> m ()
addPageTile tile = pgTiles %= (tile :)

addPageUIAction :: MonadState Page m => UI () -> m ()
addPageUIAction action = pgUIActions %= (action :)

-- Opacities used for enabled and disabled elements
enabledOpacity, disabledOpacity :: Float
enabledOpacity  = 1.0
disabledOpacity = 0.3

-- Amount of brightness changed when any brightness widget is used
brightnessChange :: Int
brightnessChange = 25 -- Relative to 255

-- Build a string for the id field in a light specific DOM object. Do this in one
-- place as we need to locate them later when we want to update

buildLightID :: LightID -> String -> String
buildLightID lightID elemName = "light-" <> fromLightID lightID <> "-" <> elemName

buildGroupID :: GroupName -> String -> String
buildGroupID groupName elemName = "light-" <> fromGroupName groupName <> "-" <> elemName

-- The getElementById function returns a Maybe, but actually just throws an exception if
-- the element is not found. The exception is unfortunately a JS exception on the client,
-- and our code just freezes / aborts without any helpful reason why the page couldn't be
-- generated. Until this is fixed in threepenny, we can only add support for tracing. Also
-- see https://github.com/HeinrichApfelmus/threepenny-gui/issues/129
getElementByIdSafe :: Window -> String -> UI Element
getElementByIdSafe window elementID = do
    -- liftIO . putStrLn $ "getElementByIdSafe: " <> elementID
    fromJust <$> getElementById window elementID

-- TODO: Those any* functions duplicate functionality already have in App.fetchBridgeState

anyLightsOn :: Lights -> Bool
anyLightsOn lights = any (^. _2 . lgtState . lsOn) $ HM.toList lights

anyLightsInGroup :: GroupName -> LightGroups -> Lights -> (Light -> Bool) -> Bool
anyLightsInGroup groupName groups lights condition =
    case HM.lookup groupName groups of
        Nothing          -> False
        Just groupLights ->
            or . map condition . catMaybes . map (flip HM.lookup lights) . HS.toList $ groupLights

-- Reload the page (TODO: Maybe we can do something more granular, just repopulate a div?)
reloadPage :: UI ()
reloadPage = runFunction $ ffi "window.location.reload(false);"

-- Apply a lens getter to the user data for the passed user ID
queryUserData :: forall a. TVar PersistConfig -> CookieUserID -> Getter UserData a -> STM a
queryUserData tvPC userID g = getUserData tvPC userID <&> (^. g)
getUserData :: TVar PersistConfig -> CookieUserID -> STM UserData
getUserData tvPC userID = readTVar tvPC <&> (^. pcUserData . at userID . non defaultUserData)

-- Captions for the show / hide group button
grpShownCaption, grpHiddenCaption :: String
grpShownCaption  = "Hide ◄"
grpHiddenCaption = "Show ►"

addEditAndDeleteButton :: String -> String -> String -> String -> H.Html
addEditAndDeleteButton editDeleteDivID
                       editBtnID
                       deleteConfirmDivID
                       deleteConfirmBtnID = do
   H.div H.! A.id (H.toValue deleteConfirmDivID)
         H.! A.class_ "btn-group btn-group-sm"
         H.! A.style "display: none;" $ do
     H.button H.! A.type_ "button"
              H.! A.id (H.toValue editBtnID)
              H.! A.class_ "btn btn-scene btn-sm"
              H.! A.onclick ( H.toValue $
                                "this.parentNode.style.display = 'none'; getElementById('"
                                <> editDeleteDivID <> "').style.display = 'block';"
                            ) $
                H.span H.! A.class_ "glyphicon glyphicon-chevron-left" $ return ()
     H.button H.! A.type_ "button"
              H.! A.id (H.toValue deleteConfirmBtnID)
              H.! A.class_ "btn btn-danger btn-sm delete-confirm-btn"
              $ "Confirm"
   H.div H.! A.id (H.toValue editDeleteDivID)
         H.! A.class_ "btn-group btn-group-sm" $ do
     H.button H.! A.type_ "button"
              H.! A.id (H.toValue editBtnID)
              H.! A.class_ "btn btn-scene btn-sm" $
                H.span H.! A.class_ "glyphicon glyphicon-th-list" $ return ()
     H.button H.! A.type_ "button"
              H.! A.class_ "btn btn-danger btn-sm delete-confirm-btn"
              H.! A.onclick ( H.toValue $
                                "this.parentNode.style.display = 'none'; getElementById('"
                                <> deleteConfirmDivID <> "').style.display = 'block';"
                            )
              $ "Delete"

