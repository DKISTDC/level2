{-# LANGUAGE ScopedTypeVariables #-}

module App.Worker.Generate.Fits where

import App.Effect.Transfer (Transfer, runTransfer)
import App.Effect.Transfer qualified as Transfer
import App.Worker.CPU (CPUWorkers (..))
import App.Worker.CPU qualified as CPU
import App.Worker.Generate.Collate (armFramesLength, collateFrames)
import App.Worker.Generate.Error (FetchError (..), GenerateError (..), generateFailed, runGenerateError)
import App.Worker.Generate.Error qualified as Gen
import App.Worker.Generate.Inputs (DownloadComplete)
import App.Worker.Generate.Level1 (canonicalL1Frames, fitsDecode, isFits, readLevel1Asdf, requireCanonicalDataset)
import Control.Monad.Catch (catch)
import Control.Monad.Loops
import Data.Either (isRight)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Time.Clock (diffUTCTime)
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.Globus (Globus, Task, TaskStatus (..), Token, Token' (Access))
import Effectful.Globus qualified as Globus
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets
import NSO.Data.Datasets qualified as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Files as Files
import NSO.Files.Image qualified as Files
import NSO.Files.Scratch as Scratch
import NSO.Image.Blanca qualified as Blanca
import NSO.Image.Fits as Fits
import NSO.Image.Fits.Quantity (QuantityError, decodeQuantitiesFrames)
import NSO.Image.Headers.Parse (requireKey, runParseError)
import NSO.Image.Headers.Types (SliceXY (..))
import NSO.Image.L1Input (L1Fits (..))
import NSO.Image.Primary (PrimaryError)
import NSO.Image.Types.Frame (Arms (..), Frames (..))
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import System.FilePath (takeFileName)
import Telescope.Data.Parser (ParseError)
import Telescope.Fits (Fits (..))
import Telescope.Fits qualified as Fits


-- -- Generate them in parallel with N = available CPUs
-- metas :: Frames (Either Skipped L2FitsMeta) <- CPU.parallelize $ fmap (workFrame task slice) gfs
--
-- log Debug $ dump "WRITTEN: " (length $ NE.filter isRight metas.frames)
-- Inversions.setGeneratedFits task.inversionId
--
-- -- generate the asdf at the end
-- Asdf.generateAsdf task.proposalId task.inversionId
-- send $ TaskSetStatus task GenAsdf

type Skipped = ()


-- | Generate a single frame
genFrame
  :: ( Time :> es
     , GenRandom :> es
     , Log :> es
     , Scratch :> es
     , Error GenerateError :> es
     , Error QuantityError :> es
     , Error ProfileError :> es
     , Error PrimaryError :> es
     )
  => Id Proposal
  -> Id Inversion
  -> SliceXY
  -> L2FrameInputs
  -> Eff es (Either Skipped L2FitsMeta)
genFrame propId invId slice frameInputs = do
  runErrorNoCallStackWith (throwError . ParseError frameInputs.l1Frame.path.filePath) . runErrorNoCallStack @Skipped $ do
    start <- currentTime
    dateBeg <- requireKey "DATE-BEG" frameInputs.l1Frame.header
    let path = Fits.outputL2Fits propId invId dateBeg

    logContext (takeFileName path.filePath) $ do
      guardAlreadyExists path
      log Debug $ "start: inputs.profiles.arms=" <> show (length frameInputs.profiles.arms)
      frame <- Fits.generateL2FrameFits start invId slice frameInputs
      let fits = Fits.frameToFits frame
      Scratch.writeFile path $ Fits.encodeL2 fits
      log Debug "WROTE"

      pure $ Fits.frameMeta frame (filenameL2Fits invId dateBeg)
 where
  guardAlreadyExists path = do
    alreadyExists <- Scratch.pathExists path
    when alreadyExists $ do
      inp <- Scratch.readFile path
      res <- runErrorNoCallStack @ParseError $ fitsDecode inp
      case res of
        -- file is bad, do not skip, regenerate
        Left _ -> pure ()
        Right fits -> do
          log Debug $ dump "SKIP frame" (length fits.extensions)
          throwError ()
