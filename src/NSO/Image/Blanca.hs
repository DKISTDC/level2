module NSO.Image.Blanca where

import Control.Exception (Exception)
import Control.Monad (foldM, zipWithM)
import Control.Monad.Catch (Handler (..), catches)
import Data.ByteString qualified as BS
import Data.List (foldl')
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array as M (Ix2 (..), IxN (..), Sz (..))
import Data.Massiv.Array qualified as M
import Effectful
import Effectful.Error.Static
import NSO.Data.Spectra (midPoint)
import NSO.Image.Headers.Types
import NSO.Image.Types.Profile
import NSO.Prelude
import NSO.Types.Wavelength (CaIILine (..), MA, Nm, SpectralLine (..), Wavelength (..))
import Telescope.Data.DataCube as DC
import Telescope.Fits as Fits


-- Decoding Profiles ---------------------------------------------------------------------------------------

-- Given the decoded blanca profiles by arm-frame, return list of frames of profiles by arm
collateFramesArms :: Arms [ProfileImage Fit] -> Arms [ProfileImage Original] -> [Arms (Profile ProfileImage)]
collateFramesArms (Arms fss) (Arms oss) =
  zipWith (\fs os -> Arms $ zipWith Profile fs os) fss oss

armsMeta :: Error BlancaError :> es => Arms [ProfileImage Fit] -> Arms [ProfileImage Original] -> Eff es (Arms (Profile ArmWavMeta))
armsMeta (Arms fs) (Arms os) =
  case Arms <$> zipWithM armMeta fs os of
    Nothing -> throwError ArmsMetaEmptyProfile
    Just arms -> pure arms

armMeta :: [ProfileImage Fit] -> [ProfileImage Original] -> Maybe (Profile ArmWavMeta)
armMeta [] _ = Nothing
armMeta _ [] = Nothing
armMeta (f:_) (o:_) = pure $ Profile {fit = f.arm, original = o.arm}

-- Decode the BLANCA output for fit profiles: inv_res_pro.fits
decodeProfileFit :: (Error BlancaError :> es) => BS.ByteString -> Eff es (Arms [ProfileImage Fit])
decodeProfileFit inp = do
  f <- decode inp
  decodeProfileArms f


-- Decode the BLANCA output for original profiles: per_ori.fits
decodeProfileOrig :: (Error BlancaError :> es) => BS.ByteString -> Eff es (Arms [ProfileImage Original])
decodeProfileOrig inp = do
  f <- decode inp
  decodeProfileArms f


decodeProfileArms :: (Error BlancaError :> es) => Fits -> Eff es (Arms [ProfileImage fit])
decodeProfileArms f = do
  hdus <- readProfileHDUs f
  metas <- profileWavMetas hdus.lineIds hdus.offsets
  let frames = profilesByFrame hdus.array
  framesByArms <- mapM (splitFrameIntoArms metas) frames
  profileArmsFrames metas framesByArms


-- Given arm meta and a list of frames, subdivided by arm, create an Arms (list of arms), split by frames
profileArmsFrames :: (Error BlancaError :> es) => Arms (ArmWavMeta fit) -> [Arms (DataCube [SlitX, Wavelength Nm, Stokes] Float)] -> Eff es (Arms [ProfileImage fit])
profileArmsFrames metas framesArms = do
  let armsFrames = L.transpose $ fmap (.arms) framesArms
  pure $ Arms $ zipWith profileArmFrames metas.arms armsFrames
 where
  profileArmFrames :: ArmWavMeta fit -> [DataCube [SlitX, Wavelength Nm, Stokes] Float] -> [ProfileImage fit]
  profileArmFrames meta = fmap (ProfileImage meta)


-- Split a single profile frame (one scan position) into N arms given N arm WavMetas
splitFrameIntoArms
  :: forall es combined arm fit
   . (Error BlancaError :> es, combined ~ [SlitX, CombinedArms (Wavelength MA), Stokes], arm ~ [SlitX, Wavelength Nm, Stokes])
  => Arms (ArmWavMeta fit)
  -> DataCube combined Float
  -> Eff es (Arms (DataCube arm Float))
splitFrameIntoArms metas wavs = do
  Arms . snd <$> foldM splitNext (wavs, []) metas
 where
  splitNext :: (DataCube combined Float, [DataCube arm Float]) -> ArmWavMeta fit -> Eff es (DataCube combined Float, [DataCube arm Float])
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


data ArmWavBreak = ArmWavBreak
  { length :: Int -- number of indices in the combined arms
  , line :: SpectralLine
  }


profileWavMetas :: (Error BlancaError :> es) => [LineId] -> [WavOffset MA] -> Eff es (Arms (ArmWavMeta fit))
profileWavMetas lids wavs = do
  breaks <- either (throwError . UnknownLineId) pure $ wavBreaks lids
  let datas = splitWavs breaks wavs
  pure $ Arms $ zipWith armWavMeta breaks.arms datas.arms


wavBreaks :: [LineId] -> Either LineId (Arms ArmWavBreak)
wavBreaks lids = do
  fromGroups $ NE.group lids
 where
  fromGroups :: [NonEmpty LineId] -> Either LineId (Arms ArmWavBreak)
  fromGroups gs = Arms <$> foldM addBreak [] gs

  addBreak :: [ArmWavBreak] -> NonEmpty LineId -> Either LineId [ArmWavBreak]
  addBreak breaks ls = do
    let lineId = NE.head ls
    line <- blancaWavLine lineId
    pure $ ArmWavBreak{length = length lids, line} : breaks

  blancaWavLine :: LineId -> Either LineId SpectralLine
  blancaWavLine = \case
    LineId 23 -> pure FeI
    LineId 9 -> pure NaD
    LineId 4 -> pure (CaII CaII_854)
    n -> Left n


splitWavs :: Arms ArmWavBreak -> [WavOffset MA] -> Arms [Wavelength Nm]
splitWavs breaks wavs =
  let (_, arms) = foldl' splitNextWav (wavs, []) breaks :: ([WavOffset MA], [[Wavelength Nm]])
   in Arms arms
 where
  splitNextWav :: ([WavOffset MA], [[Wavelength Nm]]) -> ArmWavBreak -> ([WavOffset MA], [[Wavelength Nm]])
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


armWavMeta :: ArmWavBreak -> [Wavelength Nm] -> ArmWavMeta fit
armWavMeta bk ws =
  let delta = avgDelta ws
   in ArmWavMeta
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


-- | Splits any Data Cube into frames when it is the 3rd of 4 dimension
splitFrameY :: forall a b d f. DataCube [a, b, FrameY, d] f -> [DataCube [a, b, d] f]
splitFrameY res =
  fmap sliceFrame [0 .. numFrames res - 1]
 where
  numFrames :: DataCube [a, b, FrameY, d] f -> Int
  numFrames (DataCube arr) =
    let Sz (_ :> _ :> nf :. _) = size arr
     in nf

  sliceFrame :: Int -> DataCube [a, b, d] f
  sliceFrame n = sliceM2 n res


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
  | ArmsMetaEmptyProfile
  deriving (Show, Exception, Eq)


data MassivSplitError
  = BadIndex M.IndexException
  | BadSize M.SizeException
  deriving (Show, Eq)
