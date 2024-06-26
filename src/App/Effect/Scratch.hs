module App.Effect.Scratch where

import Data.ByteString (ByteString)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FS
import Effectful.FileSystem.IO.ByteString qualified as FS
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.Inversion
import Network.Globus (Id' (Collection))
import Network.Globus qualified as Globus
import System.FilePath (takeDirectory)


data Config = Config
  { collection :: Globus.Id Collection
  , mount :: Path' Dir Scratch
  }


data Scratch :: Effect where
  ListDirectory :: Path' Dir a -> Scratch m [Path' Filename a]
  ReadFile :: Path a -> Scratch es ByteString
  WriteFile :: Path a -> ByteString -> Scratch es ()
  Globus :: Scratch es (Globus.Id Collection)
type instance DispatchOf Scratch = 'Dynamic


runScratch
  :: (FileSystem :> es)
  => Config
  -> Eff (Scratch : es) a
  -> Eff es a
runScratch cfg = interpret $ \_ -> \case
  ListDirectory dir -> listDirectory dir
  ReadFile f -> readFile f
  WriteFile f cnt -> writeFile f cnt
  Globus -> pure cfg.collection
 where
  mounted :: Path' x a -> FilePath
  mounted p = (cfg.mount </> p).filePath

  listDirectory :: (FileSystem :> es) => Path' Dir a -> Eff es [Path' Filename a]
  listDirectory dir = do
    fs <- FS.listDirectory $ mounted dir
    pure $ fmap Path fs

  readFile :: (FileSystem :> es) => Path a -> Eff es ByteString
  readFile f = do
    FS.readFile $ mounted f

  writeFile :: (FileSystem :> es) => Path a -> ByteString -> Eff es ()
  writeFile f cnt = do
    FS.createDirectoryIfMissing True $ takeDirectory (mounted f)
    FS.writeFile (mounted f) cnt


-- needs to know where the

-- DONE: ABS dir of scratch dir (App.scratch)
-- TODO: relative path from scratch globus

-- Path' Abs Scratch: the location of the scratch folder. Build locations on top of this
-- Path' Globus Scratch: the relative location of the scratch folder, from within the globus dir

baseDir :: Path' Dir Scratch
baseDir = Path "level2"


dataset :: Dataset -> Path' Dir Dataset
dataset d =
  baseDir </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.datasetId.fromId)


inversion :: Id Inversion -> Path' Dir Inversion
inversion ii =
  baseDir </> Path (cs ii.fromId)


outputL2 :: Id Inversion -> Path' Dir L2Frame
outputL2 ii = inversion ii </> Path "output"


data InvProfile
data InvResults
data OrigProfile
data Timestamps
data L2Frame


data UploadFiles t = UploadFiles
  { invProfile :: Path' t InvProfile
  , invResults :: Path' t InvResults
  , origProfile :: Path' t OrigProfile
  , timestamps :: Path' t Timestamps
  }
  deriving (Generic, Show)


inversionUploads :: Path' Dir Inversion -> UploadFiles File
inversionUploads inv =
  let invResults = inv </> fileInvResults
      invProfile = inv </> fileInvProfile
      origProfile = inv </> fileOrigProfile
      timestamps = inv </> fileTimestamps
   in UploadFiles{invResults, invProfile, origProfile, timestamps}


fileInvResults :: Path' Filename InvResults
fileInvResults = Path "inv_res_mod.fits"


fileInvProfile :: Path' Filename InvProfile
fileInvProfile = Path "inv_res_pre.fits"


fileOrigProfile :: Path' Filename OrigProfile
fileOrigProfile = Path "per_ori.fits"


fileTimestamps :: Path' Filename Timestamps
fileTimestamps = Path "timestamps.tsv"
