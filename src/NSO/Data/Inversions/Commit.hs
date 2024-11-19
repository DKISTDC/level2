module NSO.Data.Inversions.Commit where

import Data.Text qualified as Text
import Effectful
import NSO.Prelude
import NSO.Types.Inversion
import Network.HTTP.Req


validateGitCommit :: (MonadIO m) => GitRepo -> GitCommit -> m Bool
validateGitCommit (GitRepo repo) gc
  | isDebug gc = pure True
  | validHash gc = checkRemoteRepo
  | otherwise = pure False
 where
  commitUrl (GitCommit hash) =
    repo /: "commit" /: hash

  httpConfig = defaultHttpConfig{httpConfigCheckResponse = \_ _ _ -> Nothing}

  checkRemoteRepo = do
    res <- runReq httpConfig $ do
      req GET (commitUrl gc) NoReqBody ignoreResponse mempty
    pure $ responseStatusCode res == 200

  -- don't allow empty hashes or hashes shorter than 7
  validHash (GitCommit hs) = Text.length hs >= 7

  isDebug (GitCommit "DEBUG") = True
  isDebug _ = False


newtype GitRepo = GitRepo (Url Https)


vispInversionRepo :: GitRepo
vispInversionRepo = GitRepo $ https "github.com" /: "DKISTDC" /: "ViSP-Inversion"
