module NSO.Data.Scratch where

import Data.ByteString (ByteString)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FS
import Effectful.FileSystem.IO.ByteString qualified as FS
import Effectful.Log
import NSO.Prelude
import NSO.Types.Common
import Network.Globus (Id' (Collection))
import Network.Globus qualified as Globus
import System.FilePath (takeDirectory)


data Config = Config
  { collection :: Globus.Id Collection
  , mount :: Path' Dir Scratch
  }


-- this definitely feels like an app thing
-- but really, the implementation is, not the effect itself

data Scratch :: Effect where
  ListDirectory :: Path' Dir a -> Scratch m [Path' Filename a]
  ReadFile :: Path a -> Scratch es ByteString
  WriteFile :: Path a -> ByteString -> Scratch es ()
  CopyFile :: Path a -> Path a -> Scratch es ()
  PathExists :: Path a -> Scratch es Bool
  DirExists :: Path a -> Scratch es Bool
  CreateDirectoryLink :: Path' Dir a -> Path' Dir b -> Scratch es ()
  RemoveDir :: Path' Dir a -> Scratch es ()
  Globus :: Scratch es (Id Collection)
type instance DispatchOf Scratch = 'Dynamic


runScratch
  :: (FileSystem :> es, Log :> es)
  => Config
  -> Eff (Scratch : es) a
  -> Eff es a
runScratch cfg = interpret $ \_ -> \case
  ListDirectory dir -> do
    fs <- FS.listDirectory $ mounted dir
    pure $ fmap Path fs
  ReadFile f ->
    FS.readFile $ mounted f
  WriteFile f cnt -> do
    FS.createDirectoryIfMissing True $ takeDirectory (mounted f)
    FS.writeFile (mounted f) cnt
  CopyFile src dest -> do
    FS.createDirectoryIfMissing True $ takeDirectory (mounted dest)
    FS.copyFile (mounted src) (mounted dest)
  CreateDirectoryLink src dest -> do
    FS.createDirectoryIfMissing True $ takeDirectory (mounted dest)
    exists <- FS.doesDirectoryExist (mounted dest)
    when exists $ do
      FS.removeDirectoryLink (mounted dest)
    FS.createDirectoryLink (mounted src) (mounted dest)
  PathExists src -> do
    FS.doesPathExist (mounted src)
  DirExists src -> do
    FS.doesDirectoryExist (mounted src)
  RemoveDir dir -> do
    FS.removeDirectoryRecursive (mounted dir)
  Globus -> pure $ Id cfg.collection.unTagged
 where
  mounted :: Path' x a -> FilePath
  mounted p = (cfg.mount </> p).filePath


readFile :: (Scratch :> es) => Path a -> Eff es ByteString
readFile = send . ReadFile


writeFile :: (Scratch :> es) => Path a -> ByteString -> Eff es ()
writeFile f cnt = send $ WriteFile f cnt


copyFile :: (Scratch :> es) => Path a -> Path a -> Eff es ()
copyFile s d = send $ CopyFile s d


listDirectory :: (Scratch :> es) => Path' Dir a -> Eff es [Path' Filename a]
listDirectory = send . ListDirectory


pathExists :: (Scratch :> es) => Path a -> Eff es Bool
pathExists s = send $ PathExists s


symLink :: (Scratch :> es) => Path' Dir a -> Path' Dir a -> Eff es ()
symLink s d = send $ CreateDirectoryLink s d


baseDir :: Path' Dir Scratch
baseDir = Path "level2"


collection :: (Scratch :> es) => Eff es (Globus.Id Collection)
collection = do
  Id c <- send Globus
  pure $ Tagged c

-- fileTimestamps :: Path' Filename Timestamps
-- fileTimestamps = Path "timestamps.tsv"
