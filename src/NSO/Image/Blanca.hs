module NSO.Image.Blanca where

import Control.Monad (foldM)
import Control.Monad.Catch (Exception, Handler (..), catches)
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array as M (Ix2 (..), IxN (..), Sz (..))
import Data.Massiv.Array qualified as M
import Effectful
import Effectful.Error.Static
import Effectful.Log
import NSO.Image.Types.Profile
import Effectful.State.Static.Local
import NSO.Prelude
import NSO.Image.Types.Frame
import NSO.Types.Wavelength (MA, Nm, SpectralLine (..), Wavelength (..), Ion(..))
import Telescope.Data.Array (ArrayError)
import Telescope.Data.DataCube as DC
import Telescope.Fits as Fits




-- Decoding Profiles ---------------------------------------------------------------------------------------

-- Given the decoded blanca profiles by arm-frames, return list of frames each split into arms
collateFramesArms :: Arms ArmWavMeta -> Arms (Frames (ProfileImage Fit)) -> Arms (Frames (ProfileImage Original)) -> Frames (Arms ArmProfileImages)
collateFramesArms metas fits origs =
  let fitFrameArms = framesArms fits :: Frames (Arms (ProfileImage Fit))
      orgFrameArms = framesArms origs :: Frames (Arms (ProfileImage Original))
  in Frames $ NE.zipWith profile fitFrameArms.frames orgFrameArms.frames
  where
    profile :: Arms (ProfileImage Fit) -> Arms (ProfileImage Original) -> Arms ArmProfileImages
    profile fitArms origArms =
      Arms $ zipWithNE ArmProfileImages metas.arms fitArms.arms origArms.arms

    -- they MUST all be the same length
    zipWithNE :: (a->b->c->d) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d
    zipWithNE f (a :| []) (b :| _) (c :| _) = NE.singleton (f a b c)
    zipWithNE f (a :| _) (b :| []) (c :| _) = NE.singleton (f a b c)
    zipWithNE f (a :| _) (b :| _) (c :| []) = NE.singleton (f a b c)
    zipWithNE f (a :| a2 : as) (b :| b2 : bs) (c :| c2 : _cs) =
      NE.singleton (f a b c) <> zipWithNE f (a2 :| as) (b2 :| bs) (c2 :| _cs)




decodeProfileHDUs :: (Error BlancaError :> es, Log :> es) => BS.ByteString -> Eff es ProfileHDUs
decodeProfileHDUs inp = do
  fits <- Fits.decode inp
  readProfileHDUs fits



-- using the contents of either profile, decode the arm wav meta
decodeArmWavMeta :: (Error BlancaError :> es, Log :> es) => ProfileHDUs -> Eff es (Arms ArmWavMeta)
decodeArmWavMeta hdus = profileWavMetas hdus.lineIds hdus.offsets


-- decode a fit or original profile 
decodeProfileArms :: forall fit es. (Error BlancaError :> es, Log :> es) => Arms ArmWavMeta -> ProfileHDUs -> Eff es (Arms (Frames (ProfileImage fit)))
decodeProfileArms arms hdus = do
  let frames = profilesByFrame hdus.array
  case frames of
    [] -> throwError $ NoProfileFrames arms
    (f : fs) -> do
      framesByArms <- mapM (splitFrameIntoArms arms) (f :| fs)
      let imagesByArms :: Frames (Arms (ProfileImage fit)) = Frames $ fmap (\(Arms as) -> Arms $ fmap ProfileImage as) framesByArms
      pure $ armsFrames imagesByArms




-- Split a single profile frame (one scan position) into N arms given N arm WavMetas
splitFrameIntoArms
  :: forall es combined arm
   . (Error BlancaError :> es, combined ~ [SlitX, CombinedArms (Wavelength MA), Stokes], arm ~ [SlitX, Wavelength Nm, Stokes])
  => Arms ArmWavMeta
  -> DataCube combined Float
  -> Eff es (Arms (DataCube arm Float))
splitFrameIntoArms (Arms metas) wavs = do
  arms <- L.reverse . snd <$> foldM splitNext (wavs, []) metas
  case arms of
    [] -> throwError $ SplitNoArms (Arms metas)
    as -> pure $ Arms $ NE.fromList as
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


profileWavMetas :: forall es. (Error BlancaError :> es, Log :> es) => [LineId] -> [WavOffset MA] -> Eff es (Arms ArmWavMeta)
profileWavMetas [] _ = throwError NoLineIds
profileWavMetas _ [] = throwError NoWavOffsets
profileWavMetas (l : ls) wavs = do
  breaks <- either (throwError . UnknownLineId) pure $ wavBreaks (l :| ls)
  datas :: Arms (NonEmpty (WavOffset MA)) <- splitWavs breaks wavs
  pure $ Arms $ NE.zipWith armWavMeta breaks.arms (fmap NE.toList datas.arms)


wavBreaks :: NonEmpty LineId -> Either LineId (Arms ArmWavBreak)
wavBreaks lids = do
  fromGroups $ NE.group1 lids
 where
  fromGroups :: NonEmpty (NonEmpty LineId) -> Either LineId (Arms ArmWavBreak)
  fromGroups gs = do
    bs <- mapM wavBreak gs :: Either LineId (NonEmpty ArmWavBreak)
    pure $ Arms bs

  wavBreak :: NonEmpty LineId -> Either LineId ArmWavBreak
  wavBreak ls = do
    let lineId = NE.head ls
    line <- blancaWavLine lineId
    pure $ ArmWavBreak{ length = length ls, line}

  blancaWavLine :: LineId -> Either LineId SpectralLine
  blancaWavLine = \case
    LineId 23 -> pure ironLine
    LineId 9 -> pure sodiumLine
    LineId 4 -> pure calciumLine
    n -> Left n


sodiumLine :: SpectralLine
sodiumLine = SpectralLine FeI Nothing 589.29

ironLine :: SpectralLine
ironLine = SpectralLine NaI Nothing 630.3

calciumLine :: SpectralLine
calciumLine = SpectralLine CaII Nothing 854.209


splitWavs :: Error BlancaError :> es => Arms ArmWavBreak -> [WavOffset MA] -> Eff es (Arms (NonEmpty (WavOffset MA)))
splitWavs breaks wavs = do
  as <- evalState wavs $ mapM (\brk -> nextWavsN brk.length) breaks.arms
  pure $ Arms as
 where
  nextWavsN :: (State [WavOffset MA] :> es, Error BlancaError :> es) => Int -> Eff es (NonEmpty (WavOffset MA))
  nextWavsN n = do
    wos <- get @[WavOffset MA]
    case splitAt n wos of
      ([], _) -> throwError $ ShortWavs breaks wavs
      (w : ws, rest) -> do
        put rest
        pure $ w :| ws



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
pixel0 :: Foldable t => WavOffset MA -> t (WavOffset MA) -> Float
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
  | SplitNoArms (Arms ArmWavMeta)
  | ShortWavs (Arms ArmWavBreak) [WavOffset MA]
  | NoLineIds
  | NoWavOffsets
  | NoProfileFrames (Arms ArmWavMeta)
  -- | ArmsMetaEmptyProfile
  deriving (Show, Exception)


data MassivSplitError
  = BadIndex M.IndexException
  | BadSize M.SizeException
  deriving (Show, Eq)
