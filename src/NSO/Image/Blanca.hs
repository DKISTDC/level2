module NSO.Image.Blanca where

import Control.Exception (Exception)
import Control.Monad (foldM, zipWithM)
import Control.Monad.Catch (Handler (..), catch, catches)
import Data.ByteString qualified as BS
import Data.List (foldl', mapAccumL)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Ix2 (..), IxN (..), P (..), Sz (..))
import Data.Massiv.Array qualified as M
import Data.Maybe (isJust)
import Effectful
import Effectful.Error.Static
import NSO.Data.Spectra (midPoint)
import NSO.Image.Asdf.NDCollection (AlignedAxes)
import NSO.Image.Headers
import NSO.Image.Headers.Keywords
import NSO.Image.Headers.Parse
import NSO.Image.Headers.Types
import NSO.Image.Headers.WCS
import NSO.Image.Quantity (DataCommon (..), DataHDUInfo (..), DataHeader (..), addDummyAxis, dataCommon, splitFrameY)
import NSO.Image.Types.Profile (ProfileType (Fit, Original))
import NSO.Image.Types.VISPArm
import NSO.Prelude
import NSO.Types.Wavelength (CaIILine (..), MA, Nm, SpectralLine (..), Wavelength (..))
import Telescope.Asdf (ToAsdf (..))
import Telescope.Data.Axes (AxisOrder (..))
import Telescope.Data.DataCube as DC
import Telescope.Data.KnownText
import Telescope.Data.WCS
import Telescope.Fits as Fits
import Telescope.Fits.Header (Header (..), HeaderRecord (..))


-- Decoding Profiles ---------------------------------------------------------------------------------------

-- The wavelength data is combined into a single axis, containing both 630 and 854 sections
-- these are both in milliangstroms
data WavOffset unit
data Combined


-- A single arm for a single profile (fit or original)
data ProfileArm fit = ProfileArm
  { meta :: WavMeta fit
  , frames :: [DataCube [SlitX, Wavelength Nm, Stokes] Float]
  }


-- Metadata for an arm profile
data WavMeta (fit :: ProfileType) = WavMeta
  { pixel :: Float
  , delta :: Float
  , length :: Int
  , line :: SpectralLine
  }
  deriving (Show, Eq)


data WavBreak = WavBreak
  { length :: Int
  , line :: SpectralLine
  }


decodeProfileFit :: (Error BlancaError :> es) => BS.ByteString -> Eff es [ProfileArm Fit]
decodeProfileFit inp = do
  f <- decode inp
  decodeProfileArms f


decodeProfileOrig :: (Error BlancaError :> es) => BS.ByteString -> Eff es [ProfileArm Original]
decodeProfileOrig inp = do
  f <- decode inp
  decodeProfileArms f


decodeProfileArms :: (Error BlancaError :> es) => Fits -> Eff es [ProfileArm fit]
decodeProfileArms f = do
  hdus <- readProfileHDUs f
  metas <- profileWavMetas hdus.wavs hdus.offsets
  let frames = profilesByFrame hdus.array
  framesByArms <- mapM (splitFrameIntoArms metas) frames
  profileArms metas framesByArms


splitFrameIntoArms
  :: forall es profile fit
   . (Error BlancaError :> es, profile ~ [SlitX, Wavelength Nm, Stokes])
  => [WavMeta fit]
  -> DataCube profile Float
  -> Eff es (DataCube (ProfileArm fit : profile) Float)
splitFrameIntoArms metas wavs = do
  arms :: [DataCube profile Float] <- snd <$> foldM splitNext (wavs, []) metas
  sz3 <- frameSize arms
  let sz4 = frameArmSize arms sz3
  let cube :: Array D Ix4 Float = M.makeArray M.Par sz4 (toElement $ fmap (M.computeAs P . (.array)) arms)
  pure $ DataCube cube
 where
  frameArmSize :: [DataCube profile Float] -> Sz Ix3 -> Sz Ix4
  frameArmSize arms (Sz ix) = Sz $ length arms :> ix

  frameSize :: [DataCube profile Float] -> Eff es (Sz Ix3)
  frameSize [] = throwError EmptySplit
  frameSize (DataCube a : _) = do
    pure $ M.size a

  toElement :: [Array P Ix3 Float] -> Ix4 -> Float
  toElement arms (armIx :> ix3) =
    let arr = arms L.!! armIx
     in arr M.! ix3

  splitNext :: (DataCube profile Float, [DataCube profile Float]) -> WavMeta fit -> Eff es (DataCube profile Float, [DataCube profile Float])
  splitNext (combo, arms) meta = catchSplit "Profile Data" $ do
    (arm, rest) <- DC.splitM1 meta.length combo
    pure (rest, arm : arms)


profilesByFrame :: DataCube [Stokes, Wavelength Combined, FrameY, SlitX] Float -> [DataCube [SlitX, Wavelength Nm, Stokes] Float]
profilesByFrame = fmap (toNanos . swapProfileDimensions) . splitFrameY
 where
  swapProfileDimensions :: DataCube [Stokes, Wavelength Combined, SlitX] Float -> DataCube [SlitX, Wavelength Combined, Stokes] Float
  swapProfileDimensions =
    transposeMajor . transposeMinor3 . transposeMajor

  -- no conversion needed, only a type cast
  -- the wavelengths aren't encoded into the data cube, it's simply indexed by wavelength, as described in the WavMeta
  -- TODO: throw if non monotonically increasing?
  toNanos :: DataCube [SlitX, Wavelength Combined, Stokes] Float -> DataCube [SlitX, Wavelength Nm, Stokes] Float
  toNanos (DataCube arr) = DataCube arr


-- I would much rather get it right here...
profileArms :: (Error BlancaError :> es) => [WavMeta fit] -> DataCube [ProfileArm a, SlitX, Wavelength Nm, Stokes] Float -> Eff es [ProfileArm fit]
profileArms metas profiles = do
  let arms :: [[DataCube [SlitX, Wavelength Nm, Stokes] Float]] = DC.outerList profiles
  pure $ zipWith ProfileArm metas arms


data ProfileHDUs = ProfileHDUs
  { array :: DataCube '[Stokes, Wavelength Combined, FrameY, SlitX] Float
  , wavs :: DataCube '[WavIds] Int
  , offsets :: DataCube '[WavOffset MA] Float
  }


readProfileHDUs :: (Error BlancaError :> es) => Fits -> Eff es ProfileHDUs
readProfileHDUs f = do
  array <- readProfileArray
  wavs <- readWavIds
  offsets <- readWavOffsets
  pure $ ProfileHDUs{array, wavs, offsets}
 where
  readProfileArray :: Eff es (DataCube [Stokes, Wavelength Combined, FrameY, SlitX] Float)
  readProfileArray = do
    a <- decodeDataArray @Ix4 @Float f.primaryHDU.dataArray
    pure $ DataCube a

  readWavOffsets :: (Error BlancaError :> es) => Eff es (DataCube '[WavOffset MA] Float)
  readWavOffsets = do
    case f.extensions of
      -- the first extension
      (Image h : _) -> DataCube <$> decodeDataArray @Ix1 h.dataArray
      _ -> throwError $ MissingProfileExtensions "Wavelength Offsets"

  readWavIds :: (Error BlancaError :> es) => Eff es (DataCube '[WavIds] Int)
  readWavIds = do
    case f.extensions of
      -- second extension
      [_, Image h] -> do
        DataCube <$> decodeDataArray @Ix1 h.dataArray
      _ -> throwError $ MissingProfileExtensions "Wavelength Ids"


---------------------------------------------------------------
-- Wav Profiles
---------------------------------------------------------------

profileWavMetas :: (Error BlancaError :> es) => DataCube '[WavIds] Int -> DataCube '[WavOffset MA] Float -> Eff es [WavMeta fit]
profileWavMetas wids wavs = do
  breaks <- wavBreaks wids
  datas <- splitWavs breaks wavs
  pure $ zipWith wavMeta breaks datas


splitWavs :: (Error BlancaError :> es) => [WavBreak] -> DataCube '[WavOffset MA] Float -> Eff es [DataCube '[Wavelength Nm] Float]
splitWavs breaks wavs = do
  snd <$> foldM splitNextWav (wavs, []) breaks
 where
  splitNextWav (ws, wss) wb = catchSplit "Wav Offsets" $ do
    (wav, rest) <- DC.splitM0 wb.length ws
    pure (rest, offsetsToWavelengths wb.line wav : wss)


wavBreaks :: forall es. (Error BlancaError :> es) => DataCube '[WavIds] Int -> Eff es [WavBreak]
wavBreaks wds = do
  fromGroups $ NE.group (M.toList wds.array)
 where
  fromGroups :: [NonEmpty Int] -> Eff es [WavBreak]
  fromGroups = foldM addBreak []

  addBreak :: [WavBreak] -> NonEmpty Int -> Eff es [WavBreak]
  addBreak breaks wavs = do
    let wavId = NE.head wavs
    line <- blancaWavLine wavId
    pure $ WavBreak{length = length wavs, line} : breaks

  blancaWavLine :: Int -> Eff es SpectralLine
  blancaWavLine = \case
    23 -> pure FeI
    9 -> pure NaD
    4 -> pure (CaII CaII_854)
    n -> throwError $ UnknownWaveId n


wavMeta :: WavBreak -> DataCube '[Wavelength Nm] Float -> WavMeta fit
wavMeta bk (DataCube arr) =
  let ws = M.toList arr
      delta = avgDelta ws
   in WavMeta
        { delta
        , length = bk.length
        , pixel = pixel0 delta ws
        , line = bk.line
        }


offsetsToWavelengths :: SpectralLine -> DataCube '[WavOffset MA] Float -> DataCube '[Wavelength Nm] Float
offsetsToWavelengths line (DataCube a) =
  let Wavelength mid = midPoint line
   in DataCube $ M.map (offsetToWavelength $ realToFrac mid) a


-- offset in milliangstroms
offsetToWavelength :: Float -> Float -> Float
offsetToWavelength mid offset =
  mid + offset / 10000


avgDelta :: [Float] -> Float
avgDelta [] = 0
avgDelta ws = roundDigits 5 $ sum (differences ws) / fromIntegral (length ws - 1)
 where
  differences :: (Num a) => [a] -> [a]
  differences lst = zipWith (-) (drop 1 lst) lst


roundDigits :: Int -> Float -> Float
roundDigits d x = fromIntegral @Int (round $ x * 10 ^ (d :: Int)) / 10 ^ (d :: Int)


-- the interpolated pixel offset of a zero value
pixel0 :: Float -> [Float] -> Float
pixel0 dlt as =
  let mn = minimum as
   in negate mn / dlt + 1


---------------------------------------------------------------

catchSplit :: (Error BlancaError :> es) => String -> Eff es a -> Eff es a
catchSplit msg eff =
  eff
    `catches` [ Handler $ \e -> throwError $ BadSplit msg (BadIndex e)
              , Handler $ \e -> throwError $ BadSplit msg (BadSize e)
              ]


data BlancaError
  = MissingProfileExtensions String
  | UnknownWaveId Int
  | BadSplit String MassivSplitError
  | EmptySplit
  deriving (Show, Exception, Eq)


data MassivSplitError
  = BadIndex M.IndexException
  | BadSize M.SizeException
  deriving (Show, Eq)
