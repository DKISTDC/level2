module NSO.Fits.Generate.Profile where

import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString qualified as BS
import Data.Massiv.Array qualified as M
import NSO.Fits.Generate.DimArray
import NSO.Fits.Generate.Frames (splitFrames)
import NSO.Fits.Generate.Types
import NSO.Prelude
import NSO.Types.Wavelength (Wavelength)
import Telescope.Fits as Fits
import Prelude (truncate)


-- Profiles ---------------------------------------------------------------------------------------

data Original
data Fit


data ProfileFrame a = ProfileFrame
  { wav630 :: DimArray [SlitX, Wavelength 630, Stokes]
  , wav854 :: DimArray [SlitX, Wavelength 854, Stokes]
  }


data WavProfile n = WaveProfile
  { pixel :: Float
  , delta :: Float
  }


newtype WavBreakIndex = WavBreakIndex Int


data ProfileFrames a = ProfileFrames
  { frames :: [ProfileFrame a]
  , wp630 :: WavProfile 630
  , wp854 :: WavProfile 854
  }


decodeProfileFrames :: forall a m. (MonadThrow m) => BS.ByteString -> m (ProfileFrames a)
decodeProfileFrames inp = do
  f <- decode inp
  pro <- mainProfile f

  wds <- wavIds f
  bx <- indexOfWavBreak wds

  (w630, w854) <- wavs bx f
  let wp630 = wavProfile w630
      wp854 = wavProfile w854

  let pfrs = profileFrameArrays pro
  fs <- mapM (toProfileFrame bx) pfrs

  pure $ ProfileFrames fs wp630 wp854
 where
  -- mapM (fmap (uncurry ProfileFrame) . splitResultsAlongWavelength bx) $ profileFrames pro

  mainProfile :: (MonadThrow m) => Fits -> m (DimArray [Stokes, Wavs, FrameY, SlitX])
  mainProfile f = do
    a <- decodeArray @Ix4 @Float f.primaryHDU.dataArray
    pure $ DimArray a

  indexOfWavBreak :: DimArray '[WavIds] -> m WavBreakIndex
  indexOfWavBreak wds =
    case group (M.toList wds.array) of
      (w1 : _) -> pure $ WavBreakIndex (length w1)
      _ -> throwM InvalidWavelengthGroups

  wavs :: (MonadThrow m) => WavBreakIndex -> Fits -> m (DimArray '[Wavelength 630], DimArray '[Wavelength 854])
  wavs (WavBreakIndex bx) f = do
    case f.extensions of
      (Image h : _) -> do
        wvs <- DimArray <$> decodeArray @Ix1 h.dataArray

        -- WARNING: wvs is in milliangstroms
        -- TODO: convert to nm

        splitM0 bx wvs
      _ -> throwM $ MissingProfileExtensions "Wavelength Values"

  wavIds :: (MonadThrow m) => Fits -> m (DimArray '[WavIds])
  wavIds f = do
    case f.extensions of
      [_, Image h] -> do
        DimArray <$> decodeArray @Ix1 h.dataArray
      _ -> throwM $ MissingProfileExtensions "Wavelength Values"


wavProfile :: DimArray '[Wavelength n] -> WavProfile n
wavProfile (DimArray arr) =
  let ws = M.toList arr
      delta = avgDelta ws
   in WaveProfile
        { delta
        , pixel = pixel0 delta ws
        }


avgDelta :: [Float] -> Float
avgDelta ws = round5 $ sum (differences ws) / fromIntegral (length ws - 1)
 where
  differences :: (Num a) => [a] -> [a]
  differences lst = zipWith (-) (tail lst) lst

  round5 :: Float -> Float
  round5 x = fromIntegral @Int (truncate $ x * 10 ^ (5 :: Int)) / 10 ^ (5 :: Int)


pixel0 :: Float -> [Float] -> Float
pixel0 dlt as =
  let mn = minimum as
   in negate mn / dlt + 1


profileFrameArrays :: DimArray [Stokes, Wavs, FrameY, SlitX] -> [DimArray [SlitX, Wavs, Stokes]]
profileFrameArrays = fmap swapProfileDimensions . splitFrames


toProfileFrame :: (MonadThrow m) => WavBreakIndex -> DimArray [SlitX, Wavs, Stokes] -> m (ProfileFrame a)
toProfileFrame (WavBreakIndex bx) =
  fmap (uncurry ProfileFrame) <$> splitM1 bx


swapProfileDimensions :: DimArray [Stokes, Wavs, SlitX] -> DimArray [SlitX, Wavs, Stokes]
swapProfileDimensions =
  transposeMajor . transposeMinor3 . transposeMajor
