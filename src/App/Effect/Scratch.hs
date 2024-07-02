module App.Effect.Scratch where

import Data.ByteString (ByteString)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FS
import Effectful.FileSystem.IO.ByteString qualified as FS
import NSO.Fits.Generate (L2Frame, filenameL2)
import NSO.Fits.Generate.Headers.Types (DateTime (..))
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
  Globus :: Scratch es (Globus.Id Collection)
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
  Globus -> pure cfg.collection
 where
  mounted :: Path' x a -> FilePath
  mounted p = (cfg.mount </> p).filePath


readFile :: (Scratch :> es) => Path a -> Eff es ByteString
readFile = send . ReadFile


writeFile :: (Scratch :> es) => Path a -> ByteString -> Eff es ()
writeFile f cnt = send $ WriteFile f cnt


listDirectory :: (Scratch :> es) => Path' Dir a -> Eff es [Path' Filename a]
listDirectory = send . ListDirectory


baseDir :: Path' Dir Scratch
baseDir = Path "level2"


dataset :: Dataset -> Path' Dir Dataset
dataset d =
  baseDir </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.instrumentProgramId.fromId) </> Path (cs d.datasetId.fromId)


inversion :: Id Proposal -> Id Inversion -> Path' Dir Inversion
inversion ip ii =
  baseDir </> Path (cs ip.fromId) </> Path (cs ii.fromId)


outputL2Dir :: Id Proposal -> Id Inversion -> Path' Dir L2Frame
outputL2Dir ip ii = inversion ip ii </> Path "output"


outputL2Frame :: Id Proposal -> Id Inversion -> DateTime -> Path L2Frame
outputL2Frame ip ii dt =
  filePath (outputL2Dir ip ii) $ filenameL2 ii dt


data InvProfile
data InvResults
data OrigProfile
data Timestamps


data UploadFiles t = UploadFiles
  { invProfile :: Path' t InvProfile
  , invResults :: Path' t InvResults
  , origProfile :: Path' t OrigProfile
  , timestamps :: Path' t Timestamps
  }
  deriving (Generic, Show)


inversionUploads :: Path' Dir Inversion -> UploadFiles File
inversionUploads dir =
  let invResults = dir </> fileInvResults
      invProfile = dir </> fileInvProfile
      origProfile = dir </> fileOrigProfile
      timestamps = dir </> fileTimestamps
   in UploadFiles{invResults, invProfile, origProfile, timestamps}


fileInvResults :: Path' Filename InvResults
fileInvResults = Path "inv_res_mod.fits"


fileInvProfile :: Path' Filename InvProfile
fileInvProfile = Path "inv_res_pre.fits"


fileOrigProfile :: Path' Filename OrigProfile
fileOrigProfile = Path "per_ori.fits"


fileTimestamps :: Path' Filename Timestamps
fileTimestamps = Path "timestamps.tsv"
