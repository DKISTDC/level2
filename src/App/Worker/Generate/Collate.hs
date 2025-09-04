module App.Worker.Generate.Collate where

import App.Effect.Transfer (Transfer)
import App.Worker.Generate.Error (FetchError, FrameSizes (..), GenerateError (..))
import Control.Exception (Exception)
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Log
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Files
import NSO.Files.Image qualified as Files
import NSO.Files.Scratch qualified as Scratch
import NSO.Image.Blanca (BlancaError (..))
import NSO.Image.Blanca as Blanca (collateFramesArms)
import NSO.Image.Fits as Fits
import NSO.Image.Fits.Quantity (QuantityError, QuantityImage)
import NSO.Image.GWCS.L1GWCS
import NSO.Image.Headers.Parse (requireKey, runParseError)
import NSO.Image.Headers.Types (SliceXY (..), VISPArmId (..))
import NSO.Image.L1Input
import NSO.Image.Primary (PrimaryError)
import NSO.Image.Types.Frame (Arms (..), Depth, Frames (..), SlitX)
import NSO.Image.Types.Quantity
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram (Proposal)
import Network.Globus qualified as Globus
import System.FilePath (takeExtensions)
import Telescope.Asdf qualified as Asdf
import Telescope.Asdf.Error (AsdfError)
import Telescope.Data.Parser (ParseError)
import Telescope.Fits as Fits
import Telescope.Fits.Encoding as Fits


collateFrames :: (Error GenerateError :> es) => [Quantities (QuantityImage [SlitX, Depth])] -> Arms ArmWavMeta -> Arms (Frames (ProfileImage Fit)) -> Arms (Frames (ProfileImage Original)) -> [L1Fits] -> Eff es (Frames L2FrameInputs)
collateFrames qs metas pfs pos l1s = do
  unless allFramesEqual $ throwError $ MismatchedFrames frameSizes
  let pframes :: Frames (Arms ArmProfileImages) = Blanca.collateFramesArms metas pfs pos
  frames $ L.zipWith3 L2FrameInputs qs (NE.toList pframes.frames) l1s
 where
  frames [] = throwError $ NoFrames frameSizes
  frames (f : fs) = pure $ Frames $ f :| fs

  allFramesEqual :: Bool
  allFramesEqual =
    frameSizes.fit == frameSizes.quantities
      && frameSizes.original == frameSizes.quantities
      && (frameSizes.l1 - frameSizes.quantities <= 1) -- one frame may be dropped, always at the end
  frameSizes :: FrameSizes
  frameSizes =
    FrameSizes
      { quantities = length qs
      , fit = armFramesLength pfs
      , original = armFramesLength pos
      , l1 = length l1s
      }


armFramesLength :: Arms (Frames (ProfileImage fit)) -> Int
armFramesLength as = length . head $ as.arms
