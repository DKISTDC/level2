{-# LANGUAGE DefaultSignatures #-}

module App.View.Error where

import App.Colors
import App.Globus (GlobusError (..))
import Data.Aeson as A
import Effectful
import Effectful.Error.Static
import NSO.Prelude
import Web.Hyperbole
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Server


class UserFacingError err where
  viewError :: forall c. err -> View c ()
  default viewError :: forall c. (Show err) => err -> View c ()
  viewError err = do
    errorMessage "Server Error" $ do
      codeDump $ cs $ show err


userFacingError :: forall err es a. (Hyperbole :> es, UserFacingError err) => Eff (Error err : es) a -> Eff es a
userFacingError eff = do
  val <- runErrorNoCallStack @err eff
  case val of
    Left err -> do
      vw <- view $ viewError err
      send $ RespondEarly vw
    Right a -> pure a


errorMessage :: Text -> View c () -> View c ()
errorMessage msg cnt = do
  col (gap 10) $ do
    el (bold . color White . bg (light Danger) . pad 15) $ (text msg)
    col (gap 10 . pad 10) $ cnt


codeDump :: Text -> View c ()
codeDump content = code id content


instance UserFacingError GlobusError where
  viewError :: GlobusError -> View c ()
  viewError err =
    case err of
      StatusError req status body -> do
        errorMessage "GLOBUS Status Error" $ do
          codeDump $ cs $ show status
          codeDump $ cs body
          codeDump $ cs $ show req
      _ -> do
        errorMessage "GLOBUS Error" $ codeDump $ cs $ show err
