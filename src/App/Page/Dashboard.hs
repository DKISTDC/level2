module App.Page.Dashboard where

import App.Colors
import App.Globus
import App.Route
import App.Style qualified as Style
import App.Version
import App.View.Layout
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import NSO.Prelude
import Web.Hyperbole


page :: (Hyperbole :> es, Concurrent :> es, Auth :> es) => TVar (Maybe (Token Access)) -> Page es Response
page adtok = do
  -- handle admin
  load $ do
    login <- loginUrl
    mtok <- readTVarIO adtok

    appLayout Dashboard (mainView login mtok)


mainView :: Url -> Maybe (Token Access) -> View c ()
mainView login mtok = col (pad 20 . gap 20) $ do
  col id $ do
    el (fontSize 24 . bold) "Level 2"
    el_ $ text $ cs appVersion

  col id $ do
    el (bold . fontSize 18) "Admin"
    row id $ do
      case mtok of
        Nothing -> link login (Style.btnOutline Danger) "Needs Globus Login"
        Just _ -> el (color Success) "Admin Token Saved!"

-- data Admin = Admin
--   deriving (Generic, ViewId)
--
--
-- data AdminAction
--   = Login
--   deriving (Generic, ViewAction)
--
--
-- instance HyperView Admin where
--   type Action Admin = AdminAction
--
--
-- admin :: Admin -> AdminAction -> Eff es (View Admin ())
-- admin _ Login = do
--
