module App.Effect.Scratch where

import Data.ByteString (ByteString)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FS
import Effectful.FileSystem.IO.ByteString qualified as FS
import NSO.Image.Asdf (L2Asdf, filenameL2Asdf)
import NSO.Image.Frame (L2Frame, filenameL2Frame)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
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
  CopyFile :: Path a -> Path a -> Scratch es ()
  CreateDirectoryLink :: Path' Dir a -> Path' Dir b -> Scratch es ()
  Globus :: Scratch es (Id Collection)
type instance DispatchOf Scratch = 'Dynamic


runScratch
  :: (FileSystem :> es)
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
  CopyFile (Path src) dest -> do
    FS.createDirectoryIfMissing True $ takeDirectory (mounted dest)
    FS.copyFile src (mounted dest)
  CreateDirectoryLink (Path src) dest -> do
    FS.createDirectoryIfMissing True $ takeDirectory (mounted dest)
    exists <- FS.doesPathExist (mounted dest)
    unless exists $
      FS.createDirectoryLink src (mounted dest)
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


symLink :: (Scratch :> es) => Path' Dir a -> Path' Dir a -> Eff es ()
symLink s d = send $ CreateDirectoryLink s d


baseDir :: Path' Dir Scratch
baseDir = Path "level2"


input :: Path' Dir Input
input = baseDir </> "input"


generated :: Path' Dir Generated
generated = baseDir </> "generated"


dataset :: Dataset -> Path' Dir Dataset
dataset d =
  input </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.instrumentProgramId.fromId) </> Path (cs d.datasetId.fromId)


blanca :: Id Proposal -> Id Inversion -> Path' Dir BLANCA
blanca ip ii =
  input </> Path (cs ip.fromId) </> Path (cs ii.fromId)


outputL2Dir :: Id Proposal -> Id Inversion -> Path' Dir L2Frame
outputL2Dir ip ii =
  generated </> Path (cs ip.fromId) </> Path (cs ii.fromId)


outputL2Frame :: Id Proposal -> Id Inversion -> UTCTime -> Path L2Frame
outputL2Frame ip ii dt =
  filePath (outputL2Dir ip ii) $ filenameL2Frame ii dt


outputL2Asdf :: Id Proposal -> Id Inversion -> Path L2Asdf
outputL2Asdf ip ii =
  filePath (outputL2Dir ip ii) $ filenameL2Asdf ip ii


data Input


data InvProfile
data InvResults
data OrigProfile


-- data Timestamps
data BLANCA


data UploadFiles t = UploadFiles
  { invProfile :: Path' t InvProfile
  , invResults :: Path' t InvResults
  , origProfile :: Path' t OrigProfile
  -- , timestamps :: Path' t Timestamps
  }
  deriving (Generic, Show)


inversionUploads :: Path' Dir BLANCA -> UploadFiles File
inversionUploads bdir =
  let invResults = bdir </> fileInvResults
      invProfile = bdir </> fileInvProfile
      origProfile = bdir </> fileOrigProfile
   in UploadFiles{invResults, invProfile, origProfile}


-- timestamps = bdir </> fileTimestamps

fileInvResults :: Path' Filename InvResults
fileInvResults = Path "inv_res_mod.fits"


fileInvProfile :: Path' Filename InvProfile
fileInvProfile = Path "inv_res_pre.fits"


fileOrigProfile :: Path' Filename OrigProfile
fileOrigProfile = Path "per_ori.fits"

-- fileTimestamps :: Path' Filename Timestamps
-- fileTimestamps = Path "timestamps.tsv"
