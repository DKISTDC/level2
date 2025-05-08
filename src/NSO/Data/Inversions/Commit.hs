{-# LANGUAGE QuasiQuotes #-}

module NSO.Data.Inversions.Commit where

import Data.Text qualified as Text
import Effectful
import Effectful.Fetch as Fetch
import NSO.Prelude
import NSO.Types.Inversion
import Network.HTTP.Types (status200)
import Network.URI
import Network.URI.Static
import System.FilePath


validateGitCommit :: (Fetch :> es) => GitRepo -> GitCommit -> Eff es Bool
validateGitCommit (GitRepo repo) gc
  | isDebug gc = pure True
  | validHash gc = checkRemoteRepo
  | otherwise = pure False
 where
  commitUrl :: GitCommit -> URI
  commitUrl (GitCommit hash) =
    repo{uriPath = repo.uriPath </> "commit" </> cs hash}

  checkRemoteRepo :: (Fetch :> es) => Eff es Bool
  checkRemoteRepo = do
    res <- Fetch.get (commitUrl gc) [] -- this can throw, but we are sure it can be parsed so it is safe to ignore
    pure $ res.status == status200

  -- don't allow empty hashes or hashes shorter than 7
  validHash (GitCommit hs) = Text.length hs >= 7

  isDebug (GitCommit "DEBUG") = True
  isDebug _ = False


newtype GitRepo = GitRepo URI


vispInversionRepo :: GitRepo
vispInversionRepo = GitRepo $ [uri|https://github.com/DKISTDC/ViSP-Inversion|]
