{-# LANGUAGE DefaultSignatures #-}

module App.View.Error where

import App.Colors
import Effectful
import Effectful.Exception
import NSO.Prelude
import Network.Globus (GlobusError (..))
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Effect.Hyperbole


class UserFacingError err where
  viewError :: forall c. err -> View c ()
  default viewError :: forall c. (Show err) => err -> View c ()
  viewError err = do
    errorMessage "Server Error" $ do
      codeDump $ cs $ show err


userFacingError :: (Exception err, Hyperbole :> es, UserFacingError err) => err -> Eff es a
userFacingError err = do
  let r = viewResponse $ viewError err
  send $ RespondNow r


errorMessage :: Text -> View c () -> View c ()
errorMessage msg cnt = do
  col ~ gap 10 $ do
    el ~ bold . color White . bg (light Danger) . pad 15 $ text msg
    col ~ gap 10 . pad 10 $ cnt


codeDump :: Text -> View c ()
codeDump = code


instance UserFacingError GlobusError where
  viewError :: GlobusError -> View c ()
  viewError err =
    case err of
      Unauthorized req body -> do
        errorMessage "GLOBUS Unauthorized" $ do
          codeDump $ cs $ show req
          codeDump $ cs body
      ResponseBadStatus req status _ body -> do
        errorMessage "GLOBUS Status Error" $ do
          codeDump $ cs $ show status
          codeDump $ cs $ show req
          codeDump $ cs body
      ResponseBadJSON req e body -> do
        errorMessage "GLOBUS JSON Parse Error" $ do
          codeDump $ cs e
          codeDump $ cs $ show req
          codeDump $ cs body
      _ -> do
        errorMessage "GLOBUS Error" $ codeDump $ cs $ show err
