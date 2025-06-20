module NSO.Image.Blanca where

import Control.Monad (foldM, zipWithM)
import Control.Monad.Catch (Exception, Handler (..), catches)
import Data.ByteString qualified as BS
import Data.List (foldl')
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array as M (Ix2 (..), IxN (..), Sz (..))
import Data.Massiv.Array qualified as M
import Effectful
import Effectful.Error.Static
import Effectful.Log
import NSO.Data.Spectra (midPoint)
import NSO.Image.Headers.Types
import NSO.Image.Types.Profile
import NSO.Prelude
import NSO.Types.Wavelength (CaIILine (..), MA, Nm, SpectralLine (..), Wavelength (..))
import Telescope.Data.Array (ArrayError)
import Telescope.Data.DataCube as DC
import Telescope.Fits as Fits


-- Decoding Profiles ---------------------------------------------------------------------------------------

-- Given the decoded blanca profiles by arm-frames, return list of frames each split into arms
collateFramesArms :: Arms ArmWavMeta -> Arms [ProfileImage Fit] -> Arms [ProfileImage Original] -> [Arms (Profile ProfileImage)]
collateFramesArms metas fits origs =
  let fitFrames = frameArms fits :: [Arms (ProfileImage Fit)]
      orgFrames = frameArms origs :: [Arms (ProfileImage Original)]
  in zipWith profile fitFrames orgFrames
  where
    profile fitArms origArms =
      Arms $ zipWith3 Profile metas.arms fitArms.arms origArms.arms




decodeProfileHDUs :: (Error BlancaError :> es, Log :> es) => BS.ByteString -> Eff es ProfileHDUs
decodeProfileHDUs inp = do
  fits <- Fits.decode inp
  readProfileHDUs fits



-- using the contents of either profile, decode the arm wav meta
decodeArmWavMeta :: (Error BlancaError :> es, Log :> es) => ProfileHDUs -> Eff es (Arms ArmWavMeta)
decodeArmWavMeta hdus = profileWavMetas hdus.lineIds hdus.offsets


-- decode a fit or original profile 
decodeProfileArms :: forall fit es. (Error BlancaError :> es, Log :> es) => Arms ArmWavMeta -> ProfileHDUs -> Eff es (Arms [ProfileImage fit])
decodeProfileArms arms hdus = do
  let frames = profilesByFrame hdus.array
  framesByArms :: [Arms (DataCube [SlitX, Wavelength Nm, Stokes] Float)] <- mapM (splitFrameIntoArms arms) frames
  let imagesByArms :: [Arms (ProfileImage fit)] = fmap (\(Arms as) -> Arms $ fmap ProfileImage as) framesByArms
  pure $ armsFrames imagesByArms


-- Given a list of frames, subdivided by arm, create an Arms (list of arms), split by frames
armsFrames ::  [Arms a] -> Arms [a]
armsFrames frames =
  Arms $ L.transpose $ fmap (.arms) frames

frameArms ::  Arms [a] -> [Arms a]
frameArms (Arms arms) =
  fmap Arms $ L.transpose arms


-- Split a single profile frame (one scan position) into N arms given N arm WavMetas
splitFrameIntoArms
  :: forall es combined arm
   . (Error BlancaError :> es, combined ~ [SlitX, CombinedArms (Wavelength MA), Stokes], arm ~ [SlitX, Wavelength Nm, Stokes])
  => Arms ArmWavMeta
  -> DataCube combined Float
  -> Eff es (Arms (DataCube arm Float))
splitFrameIntoArms (Arms metas) wavs = do
  Arms . L.reverse . snd <$> foldM splitNext (wavs, []) metas
 where
  splitNext :: (DataCube combined Float, [DataCube arm Float]) -> ArmWavMeta -> Eff es (DataCube combined Float, [DataCube arm Float])
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

data CombinedArms a


newtype LineId = LineId Int
  deriving newtype (Eq, Show)


data ArmWavBreak = ArmWavBreak
  { line :: SpectralLine
  , length :: Int -- number of indices in the combined arms
  }
  deriving (Eq, Show)


profileWavMetas :: forall es fit. (Error BlancaError :> es, Log :> es) => [LineId] -> [WavOffset MA] -> Eff es (Arms ArmWavMeta)
profileWavMetas lids wavs = do
  breaks <- either (throwError . UnknownLineId) pure $ wavBreaks lids
  datas <- checkWavs breaks $ splitWavs breaks wavs
  pure $ Arms $ zipWith armWavMeta breaks.arms datas.arms
 where
  checkWavs :: Arms ArmWavBreak -> Arms [WavOffset MA] -> Eff es (Arms [WavOffset MA])
  checkWavs breaks aws = do
    when (any null aws.arms) $ do
      throwError $ MetaArmEmpty (length lids) (length wavs) breaks
    pure aws


wavBreaks :: [LineId] -> Either LineId (Arms ArmWavBreak)
wavBreaks lids = do
  fromGroups $ NE.group lids
 where
  fromGroups :: [NonEmpty LineId] -> Either LineId (Arms ArmWavBreak)
  fromGroups gs = Arms . L.reverse <$> foldM addBreak [] gs

  addBreak :: [ArmWavBreak] -> NonEmpty LineId -> Either LineId [ArmWavBreak]
  addBreak breaks ls = do
    let lineId = NE.head ls
    line <- blancaWavLine lineId
    pure $ ArmWavBreak{length = length ls, line} : breaks

  blancaWavLine :: LineId -> Either LineId SpectralLine
  blancaWavLine = \case
    LineId 23 -> pure FeI
    LineId 9 -> pure NaD
    LineId 4 -> pure (CaII CaII_854)
    n -> Left n


splitWavs :: Arms ArmWavBreak -> [WavOffset MA] -> Arms [WavOffset MA]
splitWavs (Arms breaks) wavs =
  let (_, arms) = foldl' splitNextWav (wavs, []) breaks :: ([WavOffset MA], [[WavOffset MA]])
   in Arms $ L.reverse arms
 where
  splitNextWav :: ([WavOffset MA], [[WavOffset MA]]) -> ArmWavBreak -> ([WavOffset MA], [[WavOffset MA]])
  splitNextWav (wos, wvs) wb =
    let wav = take wb.length wos
        rest = drop wb.length wos
     in (rest, wav : wvs)


offsetsToWavelengths :: SpectralLine -> [WavOffset MA] -> [Wavelength Nm]
offsetsToWavelengths line offs =
  let Wavelength mid = midPoint line
   in fmap (offsetToWavelength $ realToFrac mid) offs


offsetToWavelength :: Wavelength Nm -> WavOffset MA -> Wavelength Nm
offsetToWavelength (Wavelength mid) offset =
  let WavOffset offNm = toNanometers offset
   in Wavelength $ mid + realToFrac offNm / 10000


toNanometers :: WavOffset MA -> WavOffset Nm
toNanometers (WavOffset w) = WavOffset (w / 10000)


armWavMeta :: ArmWavBreak -> [WavOffset MA] -> ArmWavMeta
armWavMeta bk ws =
  let delta = avgDelta ws
   in ArmWavMeta
        { delta = toNanometers delta
        , length = bk.length
        , pixel = pixel0 delta ws
        , line = bk.line
        }


avgDelta :: [WavOffset MA] -> WavOffset MA
avgDelta [] = WavOffset 0
avgDelta ws = WavOffset $ roundDigits 5 $ sum (differences (fmap (.value) ws)) / fromIntegral (length ws - 1)
 where
  differences :: (Num a) => [a] -> [a]
  differences lst = zipWith (-) (drop 1 lst) lst


roundDigits :: (RealFrac n) => Int -> n -> n
roundDigits d x = fromIntegral @Int (round $ x * 10 ^ (d :: Int)) / 10 ^ (d :: Int)


-- the interpolated pixel offset of the value 0 in a monotonically increasing list
pixel0 :: WavOffset MA -> [WavOffset MA] -> Float
pixel0 (WavOffset dlt) as =
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


readProfileHDUs :: (Error BlancaError :> es, Log :> es) => Fits -> Eff es ProfileHDUs
readProfileHDUs f = catchSplit "readProfileHDUs" $ do
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
        fmap (LineId . round) . M.toLists <$> decodeDataArray @Ix1 @Float h.dataArray
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
    `catches` [ Handler $ \(e :: M.IndexException) -> throwError $ BadSplit msg (BadIndex e)
              , Handler $ \(e :: M.SizeException) -> throwError $ BadSplit msg (BadSize e)
              , Handler $ \(e :: ArrayError) -> throwError $ TelescopeArrayError msg e
              ]


data BlancaError
  = MissingProfileExtensions String
  | UnknownLineId LineId
  | BadSplit String MassivSplitError
  | TelescopeArrayError String ArrayError
  | MetaArmEmpty {lineIds :: Int, wavOffsets :: Int, breaks :: Arms ArmWavBreak}
  -- | ArmsMetaEmptyProfile
  deriving (Show, Exception)


data MassivSplitError
  = BadIndex M.IndexException
  | BadSize M.SizeException
  deriving (Show, Eq)
