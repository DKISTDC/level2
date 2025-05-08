module NSO.Data.Inversions.Commit where

import Control.Monad.Catch (MonadThrow)
import Data.Text qualified as Text
import Effectful
import NSO.Prelude
import NSO.Types.Inversion
import Network.HTTP.Client hiding (Proxy, Request, Response)
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (status200)
import System.FilePath


validateGitCommit :: (MonadIO m, MonadThrow m) => GitRepo -> GitCommit -> m Bool
validateGitCommit (GitRepo repo) gc
  | isDebug gc = pure True
  | validHash gc = checkRemoteRepo
  | otherwise = pure False
 where
  commitUrl :: GitCommit -> String
  commitUrl (GitCommit hash) =
    repo </> "commit" </> cs hash

  checkRemoteRepo :: (MonadIO m, MonadThrow m) => m Bool
  checkRemoteRepo = do
    req <- Http.parseRequest $ commitUrl gc -- this can throw, but we are sure it can be parsed so it is safe to ignore
    mgr <- liftIO $ Http.newManager defaultManagerSettings
    res <- liftIO $ Http.httpLbs req mgr
    pure $ responseStatus res == status200

  -- don't allow empty hashes or hashes shorter than 7
  validHash (GitCommit hs) = Text.length hs >= 7

  isDebug (GitCommit "DEBUG") = True
  isDebug _ = False


newtype GitRepo = GitRepo String


vispInversionRepo :: GitRepo
vispInversionRepo = GitRepo "https://github.com/DKISTDC/ViSP-Inversion"
