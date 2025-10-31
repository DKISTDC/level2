{-# LANGUAGE ScopedTypeVariables #-}

module App.Worker.Generate.Fits where

import App.Worker.Generate.Decode
import App.Worker.Generate.Error (FetchError (..), GenerateError (..))
import Effectful
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.Log
import Effectful.Time
import NSO.Data.Inversions as Inversions
import NSO.Files as Files
import NSO.Files.Image as Files
import NSO.Files.Scratch as Scratch
import NSO.Image.Fits as Fits
import NSO.Image.Fits.Quantity (QuantityError)
import NSO.Image.Headers.Parse (requireKey)
import NSO.Image.Headers.Types (SliceXY (..))
import NSO.Image.L1Input (L1Fits (..))
import NSO.Image.Primary (PrimaryError)
import NSO.Image.Types.Frame (Arms (..))
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import System.FilePath (takeFileName)
import Telescope.Data.Parser (ParseError)


type Skipped = ()


-- | Generate a single frame
genFrame
  :: ( Time :> es
     , GenRandom :> es
     , Log :> es
     , Scratch :> es
     , Error GenerateError :> es
     , Error FetchError :> es
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

    let ctx = takeFileName path.filePath
    logContext ctx $ do
      guardAlreadyExists path
      logStatus $ "generating... arms=" <> show (length frameInputs.profiles.arms)
      !frame <- Fits.generateL2FrameFits start invId slice frameInputs
      let !fits = Fits.frameToFits frame
      Scratch.writeFile path $ Fits.encodeL2 fits
      logStatus "WROTE"

      pure $ Fits.frameMeta frame (Files.filenameL2Fits invId dateBeg)
 where
  guardAlreadyExists path = do
    alreadyExists <- Scratch.pathExists path
    when alreadyExists $ do
      inp <- Scratch.readFile path
      res <- runErrorNoCallStack @ParseError $ fitsDecode inp
      case res of
        -- file is bad, do not skip, regenerate
        Left _ -> pure ()
        Right _fits -> do
          -- log Debug $ dump "SKIP frame" (length fits.extensions)
          throwError ()
