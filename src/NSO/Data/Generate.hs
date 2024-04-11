{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module NSO.Data.Generate where

import Data.ByteString qualified as BS

import Data.Kind
import Data.Massiv.Array hiding (mapM, mapM_, transposeOuter)
import Effectful
import Effectful.Writer.Static.Local
import NSO.Prelude
import Telescope.Fits as Fits
import Telescope.Fits.Header
import Telescope.Fits.Types as Fits


------------------------------------------------------------------------------

testInput :: FilePath
testInput = "/Users/seanhess/Data/scan1807/inv_res_mod.fits"


test :: IO ()
test = do
  putStrLn "TEST"
  (f : fs) <- readQuantitiesFrames testInput

  print $ f.temperature !> 1

  let dat = encodeArray f.opticalDepth
  print dat.axes
  print dat.bitpix
  print $ BS.length dat.rawData

  let fits = quantitiesFits f
  print $ length fits.extensions

  let (Image i : _) = fits.extensions
  -- print $ BS.length i.dataArray.rawData

  print fits.primaryHDU.dataArray.rawData

  let out = encode fits
  BS.writeFile "/Users/seanhess/code/notebooks/data/out.fits" out


-- Quantiies To Fits -------------------------------------------------

data Quantities = Quantities
  { opticalDepth :: Array D Ix2 Float
  , temperature :: Array D Ix2 Float
  , electronPressure :: Array D Ix2 Float
  , microTurbulence :: Array D Ix2 Float
  , magStrength :: Array D Ix2 Float
  , velocity :: Array D Ix2 Float
  , magInclination :: Array D Ix2 Float
  , magAzimuth :: Array D Ix2 Float
  , geoHeight :: Array D Ix2 Float
  , gasPressure :: Array D Ix2 Float
  , density :: Array D Ix2 Float
  }


quantitiesFits :: Quantities -> Fits
quantitiesFits q = Fits primaryHDU $ fmap Image $ quantitiesHDUs q


-- What is supposed to go in here?
primaryHDU :: PrimaryHDU
primaryHDU = PrimaryHDU primaryHeaders emptyDataArray
 where
  primaryHeaders =
    Header
      [keyword "WOOT" (String "this is a very long string passing 30 characters") Nothing, keyword "CUSTOM" (String "adf") Nothing]


-- TODO: generate a bunch of HDUs!
quantitiesHDUs :: Quantities -> [ImageHDU]
quantitiesHDUs q = runPureEff . execWriter $ do
  opticalDepth
  temperature
  electronPressure
  microTurbulence
  magStrength
  velocity
  magInclination
  magAzimuth
  geoHeight
  gasPressure
  density
 where
  -- how do you know which is which?
  dataHDU :: (Writer [ImageHDU] :> es) => Text -> Array D Ix2 Float -> Eff es ()
  dataHDU ext arr = do
    let header = Header $ dataHDUHeaders ext
        dataArray = encodeArray arr
    tell [ImageHDU{header, dataArray}]

  dataHDUHeaders :: Text -> [HeaderRecord]
  dataHDUHeaders ext =
    [ extName ext
    , custom "woot"
    ]

  extName n = keyword "EXTNAME" (String n) Nothing
  custom n = keyword "CUSTOM" (String n) Nothing

  opticalDepth =
    dataHDU "Log of optical depth at 500nm" q.opticalDepth

  temperature =
    dataHDU "Temperature" q.temperature

  electronPressure =
    dataHDU "Electron Pressure" q.electronPressure

  microTurbulence =
    dataHDU "Microturbulence" q.microTurbulence

  magStrength =
    dataHDU "Magnetic Field Strength" q.magStrength

  velocity =
    dataHDU "Line-of-sight Velocity" q.velocity

  magInclination =
    dataHDU "Magnetic Field Inclination (w.r.t. line-of-sight)" q.magInclination

  magAzimuth =
    dataHDU "Magnetic Field Azimuth (w.r.t. line-of-sight)" q.magAzimuth

  geoHeight =
    dataHDU "Geometric Height above solar surface (tau ~ 1 at 500nm)" q.geoHeight

  gasPressure =
    dataHDU "Gas Pressure" q.gasPressure

  density =
    dataHDU "Density" q.density


-- Parse Quantities ---------------------------------------------------------------------------------

readQuantitiesFrames :: (MonadIO m, MonadThrow m) => FilePath -> m [Quantities]
readQuantitiesFrames fp = do
  inp <- liftIO $ BS.readFile fp
  res <- decodeResults inp
  resultsQuantities res


decodeResults :: (MonadThrow m) => BS.ByteString -> m (Results [Quantity, Depth, FrameY, SlitX])
decodeResults inp = do
  f <- decode inp
  a <- decodeArray @Ix4 @Float f.primaryHDU.dataArray
  pure $ Results a


resultsQuantities :: (MonadThrow m) => Results [Quantity, Depth, FrameY, SlitX] -> m [Quantities]
resultsQuantities res = do
  fs <- resultsByFrame res
  mapM splitQuantitiesM fs


resultsByFrame :: (MonadThrow m) => Results [Quantity, Depth, FrameY, SlitX] -> m [Results [Quantity, Depth, SlitX]]
resultsByFrame res =
  let maxFrame = numFrames res - 1
   in mapM (sliceFrame res) [0 .. maxFrame]
 where
  numFrames :: Results [Quantity, Depth, FrameY, SlitX] -> Int
  numFrames (Results arr) =
    let Sz (_ :> _ :> nf :. _) = size arr
     in nf

  sliceFrame :: (MonadThrow m) => Results [Quantity, Depth, FrameY, SlitX] -> Int -> m (Results [Quantity, Depth, SlitX])
  sliceFrame (Results arr) n =
    case arr <!?> (Dim 2, n) of
      Nothing -> throwM $ FrameOutOfBounds (size arr) n
      Just s -> pure $ Results s


splitQuantitiesM :: (MonadThrow m) => Results [Quantity, Depth, SlitX] -> m Quantities
splitQuantitiesM rbf =
  case splitQuantities rbf of
    Nothing -> throwM $ InvalidFrameShape (size rbf.array)
    Just qs -> pure qs


splitQuantities :: Results [Quantity, Depth, SlitX] -> Maybe Quantities
splitQuantities res = do
  let qs = fmap (.array) $ outerList res
  [opticalDepth, temperature, electronPressure, microTurbulence, magStrength, velocity, magInclination, magAzimuth, geoHeight, gasPressure, density] <- pure qs
  pure Quantities{..}


-- Results ------------------------------------------------------------------------------

newtype Results (as :: [Type]) = Results
  { array :: Array D (ResultsIx as) Float
  }


instance (Ragged L (ResultsIx as) Float) => Show (Results as) where
  show (Results a) = show a


data Depth
data SlitX
data FrameY
data Quantity
class IsResults (as :: [Type]) where
  type ResultsIx as :: Type
  outerLength :: Results as -> Int


instance IsResults '[a] where
  type ResultsIx '[a] = Ix1
  outerLength (Results a) =
    let Sz s = size a in s


instance IsResults '[a, b] where
  type ResultsIx '[a, b] = Ix2
  outerLength (Results a) =
    let Sz (s :. _) = size a in s


instance IsResults '[a, b, c] where
  type ResultsIx '[a, b, c] = Ix3
  outerLength (Results a) =
    let Sz (s :> _) = size a in s


instance IsResults '[a, b, c, d] where
  type ResultsIx '[a, b, c, d] = Ix4
  outerLength (Results a) =
    let Sz (s :> _) = size a in s


outerList :: forall a as. (Lower (ResultsIx (a : as)) ~ ResultsIx as, Index (ResultsIx as), Index (ResultsIx (a : as))) => Results (a : as) -> [Results as]
outerList (Results a) = foldOuterSlice row a
 where
  row :: Array D (ResultsIx as) Float -> [Results as]
  row r = [Results r]


-- Errors ------------------------------------------

data GenerateError
  = InvalidFrameShape (Sz Ix3)
  | InvalidFits String
  | FrameOutOfBounds (Sz Ix4) Int
  deriving (Show, Eq, Exception)
