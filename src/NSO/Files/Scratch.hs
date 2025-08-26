module NSO.Files.Scratch where

import Data.ByteString (ByteString)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FS
import Effectful.FileSystem.IO.ByteString qualified as FS
import Effectful.Log
import NSO.Files.RemoteFolder
import NSO.Prelude
import NSO.Types.Common
import Network.Globus (Id' (Collection))
import Network.Globus qualified as Globus
import System.FilePath (takeDirectory)


data Config = Config
  { collection :: Globus.Id Collection
  , mount :: Path Scratch Dir ()
  }


data Mounted a


data Scratch :: Effect where
  ListDirectory :: Path Scratch Dir a -> Scratch m [Path Scratch Filename a]
  ReadFile :: Path Scratch File a -> Scratch es ByteString
  WriteFile :: Path Scratch File a -> ByteString -> Scratch es ()
  CopyFile :: Path Scratch File a -> Path Scratch File a -> Scratch es ()
  PathExists :: Path Scratch File a -> Scratch es Bool
  DirExists :: Path Scratch File a -> Scratch es Bool
  CreateDirectoryLink :: Path Scratch Dir a -> Path Scratch Dir b -> Scratch es ()
  RemoveDir :: Path Scratch Dir a -> Scratch es ()
  Globus :: Scratch es (Id Collection)
  MountedPath :: Path Scratch x a -> Scratch es (Path Scratch (Mounted x) a)
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
    exists <- FS.doesDirectoryExist (mounted dir)
    when exists $ do
      FS.removeDirectoryRecursive (mounted dir)
  Globus -> pure $ Id cfg.collection.unTagged
  MountedPath p -> pure $ mounted' p
 where
  mounted :: Path Scratch x a -> FilePath
  mounted p = (mounted' p).filePath

  mounted' :: Path Scratch x a -> Path Scratch (Mounted x) a
  mounted' p = cfg.mount </> p


readFile :: (Scratch :> es) => Path Scratch File a -> Eff es ByteString
readFile = send . ReadFile


writeFile :: (Scratch :> es) => Path Scratch File a -> ByteString -> Eff es ()
writeFile f cnt = send $ WriteFile f cnt


copyFile :: (Scratch :> es) => Path Scratch File a -> Path Scratch File a -> Eff es ()
copyFile s d = send $ CopyFile s d


listDirectory :: (Scratch :> es) => Path Scratch Dir a -> Eff es [Path Scratch Filename a]
listDirectory = send . ListDirectory


pathExists :: (Scratch :> es) => Path Scratch File a -> Eff es Bool
pathExists s = send $ PathExists s


symLink :: (Scratch :> es) => Path Scratch Dir a -> Path Scratch Dir a -> Eff es ()
symLink s d = send $ CreateDirectoryLink s d


baseDir :: Path Scratch Dir ()
baseDir = Path "level2"


mountedPath :: (Scratch :> es) => Path Scratch x a -> Eff es (Path Scratch (Mounted x) a)
mountedPath = send . MountedPath


collection :: (Scratch :> es) => Eff es (Globus.Id Collection)
collection = do
  Id c <- send Globus
  pure $ Tagged c


-- Remote Folder ----------------------------------------

remote :: (Scratch :> es) => Path Scratch Dir a -> Eff es (RemoteFolder Scratch a)
remote dir = do
  scratch <- collection
  Path mpath <- mountedPath dir
  pure $ RemoteFolder scratch (Path mpath)
