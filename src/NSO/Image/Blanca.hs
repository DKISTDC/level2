module NSO.Image.Blanca where

import Control.Exception (Exception)
import Control.Monad (foldM, zipWithM)
import Control.Monad.Catch (Handler (..), catch, catches)
import Data.ByteString qualified as BS
import Data.List (foldl', mapAccumL)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Ix2 (..), IxN (..), Sz (..))
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
import NSO.Image.Types.Profile
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
  metas <- profileWavMetas hdus.lineIds hdus.offsets
  let frames = profilesByFrame hdus.array
  framesByArms <- mapM (splitFrameIntoArms metas) frames
  profileArms metas framesByArms


profileArms :: (Error BlancaError :> es) => [WavMeta fit] -> [Arms (DataCube [SlitX, Wavelength Nm, Stokes] Float)] -> Eff es [ProfileArm fit]
profileArms metas framesArms = do
  let armsFrames = L.transpose $ fmap (.arms) framesArms
  pure $ zipWith ProfileArm metas armsFrames


splitFrameIntoArms
  :: forall es combined arm fit
   . (Error BlancaError :> es, combined ~ [SlitX, CombinedArms (Wavelength MA), Stokes], arm ~ [SlitX, Wavelength Nm, Stokes])
  => [WavMeta fit]
  -> DataCube combined Float
  -> Eff es (Arms (DataCube arm Float))
splitFrameIntoArms metas wavs = do
  Arms . snd <$> foldM splitNext (wavs, []) metas
 where
  splitNext :: (DataCube combined Float, [DataCube arm Float]) -> WavMeta fit -> Eff es (DataCube combined Float, [DataCube arm Float])
  splitNext (combo, arms) meta = catchSplit "Profile Data" $ do
    (arm, rest) <- DC.splitM1 meta.length combo
    pure (rest, toNanos arm : arms)

  -- no conversion needed, only a type cast
  -- the wavelengths aren't encoded into the data cube, it's simply indexed by wavelength, as described in the WavMeta
  -- TODO: throw if non monotonically increasing
  toNanos :: DataCube combined Float -> DataCube arm Float
  toNanos (DataCube arr) = DataCube arr


-- | Split 4D data into list of frames of 3D data corresponding to each scan position (FrameY)
profilesByFrame :: DataCube [Stokes, CombinedArms (Wavelength MA), FrameY, SlitX] Float -> [DataCube [SlitX, CombinedArms (Wavelength MA), Stokes] Float]
profilesByFrame = fmap swapProfileDimensions . splitFrameY
 where
  swapProfileDimensions :: DataCube [Stokes, CombinedArms (Wavelength MA), SlitX] Float -> DataCube [SlitX, CombinedArms (Wavelength MA), Stokes] Float
  swapProfileDimensions =
    transposeMajor . transposeMinor3 . transposeMajor


---------------------------------------------------------------
-- Wav Profiles
---------------------------------------------------------------

-- BLANCA encodes wavelength information in the 2nd two HDUs. The information for each arm is encoded into one 1D array
--  1. Spectral Line Ids [23, 23, 23, 23, 4, 4] - ids describing which line was used
--  2. Wavelength Offsets [-0.1, 0.0, 0.1, 0.2, 0, 0.1] - how far from the center of the spectral line this index is in milliangstroms

newtype WavOffset unit = WavOffset Float
data CombinedArms a


newtype LineId = LineId Int
  deriving (Eq, Show)


data WavBreak = WavBreak
  { length :: Int -- number of indices in the combined arms
  , line :: SpectralLine
  }


profileWavMetas :: (Error BlancaError :> es) => [LineId] -> [WavOffset MA] -> Eff es [WavMeta fit]
profileWavMetas lids wavs = do
  breaks <- either (throwError . UnknownLineId) pure $ wavBreaks lids
  let datas = splitWavs breaks wavs
  pure $ zipWith wavMeta breaks.arms datas.arms


wavBreaks :: [LineId] -> Either LineId (Arms WavBreak)
wavBreaks lids = do
  fromGroups $ NE.group lids
 where
  fromGroups :: [NonEmpty LineId] -> Either LineId (Arms WavBreak)
  fromGroups gs = Arms <$> foldM addBreak [] gs

  addBreak :: [WavBreak] -> NonEmpty LineId -> Either LineId [WavBreak]
  addBreak breaks ls = do
    let lineId = NE.head ls
    line <- blancaWavLine lineId
    pure $ WavBreak{length = length lids, line} : breaks

  blancaWavLine :: LineId -> Either LineId SpectralLine
  blancaWavLine = \case
    LineId 23 -> pure FeI
    LineId 9 -> pure NaD
    LineId 4 -> pure (CaII CaII_854)
    n -> Left n


splitWavs :: Arms WavBreak -> [WavOffset MA] -> Arms [Wavelength Nm]
splitWavs breaks wavs =
  let (_, arms) = foldl' splitNextWav (wavs, []) breaks :: ([WavOffset MA], [[Wavelength Nm]])
   in Arms arms
 where
  splitNextWav :: ([WavOffset MA], [[Wavelength Nm]]) -> WavBreak -> ([WavOffset MA], [[Wavelength Nm]])
  splitNextWav (wos, wvs) wb =
    let wav = take wb.length wos
        rest = drop wb.length wos
     in (rest, offsetsToWavelengths wb.line wav : wvs)

  offsetsToWavelengths :: SpectralLine -> [WavOffset MA] -> [Wavelength Nm]
  offsetsToWavelengths line offs =
    let Wavelength mid = midPoint line
     in fmap (offsetToWavelength $ realToFrac mid) offs

  -- offset in milliangstroms
  offsetToWavelength :: Wavelength Nm -> WavOffset MA -> Wavelength Nm
  offsetToWavelength (Wavelength mid) (WavOffset offset) =
    Wavelength $ mid + realToFrac offset / 10000


wavMeta :: WavBreak -> [Wavelength Nm] -> WavMeta fit
wavMeta bk ws =
  let delta = avgDelta ws
   in WavMeta
        { delta
        , length = bk.length
        , pixel = pixel0 delta ws
        , line = bk.line
        }


avgDelta :: [Wavelength Nm] -> Wavelength Nm
avgDelta [] = 0
avgDelta ws = Wavelength $ roundDigits 5 $ sum (differences (fmap (.value) ws)) / fromIntegral (length ws - 1)
 where
  differences :: (Num a) => [a] -> [a]
  differences lst = zipWith (-) (drop 1 lst) lst


roundDigits :: (RealFrac n) => Int -> n -> n
roundDigits d x = fromIntegral @Int (round $ x * 10 ^ (d :: Int)) / 10 ^ (d :: Int)


-- the interpolated pixel offset of the value 0 in a monotonically increasing list
pixel0 :: Wavelength Nm -> [Wavelength Nm] -> Double
pixel0 (Wavelength dlt) as =
  let mn = minimum as
   in negate mn.value / dlt + 1


---------------------------------------------------------------------
-- BLANCA Fits Profile Output
---------------------------------------------------------------------

data ProfileHDUs = ProfileHDUs
  { array :: DataCube '[Stokes, CombinedArms (Wavelength MA), FrameY, SlitX] Float
  , lineIds :: [LineId] -- combined  arms spectral line ids
  , offsets :: [WavOffset MA] -- combined arms wavelength offsets
  }


readProfileHDUs :: (Error BlancaError :> es) => Fits -> Eff es ProfileHDUs
readProfileHDUs f = do
  array <- readProfileArray
  lineIds <- readLineIds
  offsets <- readWavOffsets
  pure $ ProfileHDUs{array, lineIds, offsets}
 where
  readProfileArray :: Eff es (DataCube [Stokes, CombinedArms (Wavelength MA), FrameY, SlitX] Float)
  readProfileArray = do
    a <- decodeDataArray @Ix4 @Float f.primaryHDU.dataArray
    pure $ DataCube a

  readWavOffsets :: (Error BlancaError :> es) => Eff es [WavOffset MA]
  readWavOffsets = do
    case f.extensions of
      -- the first extension
      (Image h : _) -> fmap WavOffset . M.toLists <$> decodeDataArray @Ix1 h.dataArray
      _ -> throwError $ MissingProfileExtensions "Wavelength Offsets"

  readLineIds :: (Error BlancaError :> es) => Eff es [LineId]
  readLineIds = do
    case f.extensions of
      -- second extension
      [_, Image h] -> do
        fmap LineId . M.toLists <$> decodeDataArray @Ix1 h.dataArray
      _ -> throwError $ MissingProfileExtensions "Wavelength Ids"


-- Errors ------------------------------------------------------------------

catchSplit :: (Error BlancaError :> es) => String -> Eff es a -> Eff es a
catchSplit msg eff =
  eff
    `catches` [ Handler $ \e -> throwError $ BadSplit msg (BadIndex e)
              , Handler $ \e -> throwError $ BadSplit msg (BadSize e)
              ]


data BlancaError
  = MissingProfileExtensions String
  | UnknownLineId LineId
  | BadSplit String MassivSplitError
  deriving (Show, Exception, Eq)


data MassivSplitError
  = BadIndex M.IndexException
  | BadSize M.SizeException
  deriving (Show, Eq)
