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
import NSO.Image.Quantity (DataCommon (..), DataHDUInfo (..), DataHeader (..), addDummyAxis, dataCommon, splitFrames)
import NSO.Image.Types.Profile (ProfileType (Fit, Original))
import NSO.Image.Types.VISPArm
import NSO.Prelude
import NSO.Types.Wavelength (CaIILine (..), MA, Nm, SpectralLine (..), Wavelength (..))
import Telescope.Asdf (ToAsdf (..))
import Telescope.Data.Axes (AxisOrder (..))
import Telescope.Data.DataCube
import Telescope.Data.KnownText
import Telescope.Data.WCS
import Telescope.Fits as Fits
import Telescope.Fits.Header (Header (..), HeaderRecord (..))


-- Decoding Profiles ---------------------------------------------------------------------------------------

-- The wavelength data is combined into a single axis, containing both 630 and 854 sections
-- these are both in milliangstroms
data Wavs


-- data ProfileFrame profile = ProfileFrame
--   { armFe :: DataCube [SlitX, Wavelength (VISPArm ArmFeI Nm), Stokes] Float
--   , armCa :: DataCube [SlitX, Wavelength (VISPArm ArmFeI Nm), Stokes] Float
--   , armNa :: DataCube [SlitX, Wavelength (VISPArm ArmFeI Nm), Stokes] Float
--   }

-- Metadata for profiles

data ProfileArm fit = ProfileArm
  { frames :: [DataCube [SlitX, Wavelength Nm, Stokes] Float]
  , meta :: WavMeta fit
  }


data WavMeta (fit :: ProfileType) = WavMeta
  { pixel :: Float
  , delta :: Float
  , line :: SpectralLine
  }
  deriving (Show, Eq)


data WavBreak = WavBreak
  { length :: Int
  , line :: SpectralLine
  }


-- data ProfileFrames (fit :: ProfileType) = ProfileFrames
--   { armFeI :: ProfileArmFrames fit ArmFeI
--   , armNaD :: ProfileArmFrames fit ArmNaD
--   , armCa854 :: ProfileArmFrames fit ArmCa854
--   }

-- data WavProfiles profile = WavProfiles
--   { wav630 :: WavProfile Wav630
--   , wav854 :: WavProfile Wav854
--   }
--   deriving (Show, Eq)

decodeProfileFit :: (Error BlancaError :> es) => BS.ByteString -> Eff es [ProfileArm Fit]
decodeProfileFit inp = do
  f <- decode inp
  profileArms f


decodeProfileOrig :: (Error BlancaError :> es) => BS.ByteString -> Eff es [ProfileArm Original]
decodeProfileOrig inp = do
  f <- decode inp
  profileArms f


profileArms :: (Error BlancaError :> es) => Fits -> Eff es [ProfileArm fit]
profileArms f = do
  pro <- mainProfile

  wids <- readWavIds f.extensions
  wavs <- readWavs f.extensions
  metas <- profileWavMetas wids wavs
  _
 where
  -- let wp630 = wavMeta FeI w630
  --     wp854 = wavMeta (CaII CaII_854) w854
  --
  -- let pfrs = profileFrameArrays pro
  -- fs <- mapM (toProfileFrameArm bx) pfrs
  --
  -- pure $ ProfileFrames fs (WavProfiles wp630 wp854)

  mainProfile :: Eff es (DataCube [Stokes, Wavs, FrameY, SlitX] Float)
  mainProfile = do
    a <- decodeDataArray @Ix4 @Float f.primaryHDU.dataArray
    pure $ DataCube a

  readWavs :: (Error BlancaError :> es) => [Fits.Extension] -> Eff es (DataCube '[Wavelength MA] Float)
  readWavs exts = do
    case exts of
      -- the first extensions
      (Image h : _) -> DataCube <$> decodeDataArray @Ix1 h.dataArray
      _ -> throwError $ MissingProfileExtensions "Wavelength Values"

  readWavIds :: (Error BlancaError :> es) => [Fits.Extension] -> Eff es (DataCube '[WavIds] Int)
  readWavIds exts = do
    case exts of
      -- should be the second extension
      [_, Image h] -> do
        DataCube <$> decodeDataArray @Ix1 h.dataArray
      _ -> throwError $ MissingProfileExtensions "Wavelength Ids"

  -- -- each one is a data cube
  -- toProfileFrame :: WavBreakIndex -> DataCube [SlitX, Wavs, Stokes] Float -> Eff es (VISPArms fit)
  -- toProfileFrame (WavBreakIndex bx) da = do
  --   -- splitM1..... erm.... might get more complicated
  --   (w630, w854) <- splitM1 bx da
  --   pure $ VISPArms (fromWavs w630) (fromWavs w854)

  -- we can convert from Wavs to (Wavelength n), because there isn't any wavelength data here
  -- it's just one of the pixel axes. And we've successfully split them up here
  fromWavs (DataCube arr) = DataCube arr


profileFrameArrays :: DataCube [Stokes, Wavs, FrameY, SlitX] Float -> [DataCube [SlitX, Wavs, Stokes] Float]
profileFrameArrays = fmap swapProfileDimensions . splitFrames


swapProfileDimensions :: DataCube [Stokes, Wavs, SlitX] Float -> DataCube [SlitX, Wavs, Stokes] Float
swapProfileDimensions =
  transposeMajor . transposeMinor3 . transposeMajor


---------------------------------------------------------------
-- Wav Profiles
---------------------------------------------------------------

profileWavMetas :: DataCube '[WavIds] Int -> DataCube '[Wavelength MA] Float -> Eff es [WavMeta fit]
profileWavMetas wids wavs = do
  breaks <- wavBreaks wids
  datas <- splitWavs breaks wavs
  pure $ zipWith wavMeta (fmap (.line) breaks) datas


splitWavs :: [WavBreak] -> DataCube '[Wavelength MA] Float -> Eff es [DataCube '[Wavelength Nm] Float]
splitWavs breaks wavs =
  snd <$> foldM splitNextWav (wavs, []) breaks
 where
  splitNextWav (ws, wss) wb = do
    (wav, rest) <-
      splitM0 wb.length ws
        `catches` [ badWavSplit BadIndex
                  , badWavSplit BadSize
                  ]
    pure (rest, wavsToNanometers wav : wss)

  badWavSplit f = Handler $ \e -> throwError $ BadWavSplit $ f e


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


wavMeta :: SpectralLine -> DataCube '[Wavelength Nm] Float -> WavMeta fit
wavMeta l (DataCube arr) =
  let ws = M.toList arr
      delta = avgDelta ws
   in WavMeta
        { delta
        , pixel = pixel0 delta ws
        , line = l
        }


wavsToNanometers :: DataCube '[Wavelength MA] Float -> DataCube '[Wavelength Nm] Float
wavsToNanometers (DataCube a) = DataCube $ M.map (/ 10000) a


avgDelta :: [Float] -> Float
avgDelta [] = 0
avgDelta ws = round5 $ sum (differences ws) / fromIntegral (length ws - 1)
 where
  differences :: (Num a) => [a] -> [a]
  differences lst = zipWith (-) (drop 1 lst) lst


round5 :: Float -> Float
round5 x = fromIntegral @Int (round $ x * 10 ^ (5 :: Int)) / 10 ^ (5 :: Int)


-- the interpolated pixel offset of a zero value
pixel0 :: Float -> [Float] -> Float
pixel0 dlt as =
  let mn = minimum as
   in negate mn / dlt + 1


---------------------------------------------------------------

data BlancaError
  = MissingProfileExtensions String
  | UnknownWaveId Int
  | BadWavSplit MassivSplitError
  deriving (Show, Exception, Eq)


data MassivSplitError
  = BadIndex M.IndexException
  | BadSize M.SizeException
  deriving (Show, Eq)
