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
  putStrLn "TEST?"
  (f : fs) <- readQuantitiesFrames testInput

  putStrLn "WOOO"
  print $ size $ f.temperature.array


-- print $ f.temperature.array !> 1

-- let dat = encodeArray f.opticalDepth.array
-- print dat.axes
-- print dat.bitpix
-- print $ BS.length dat.rawData
--
-- let fits = quantitiesFits f
-- print $ length fits.extensions
--
-- let (Image i : _) = fits.extensions
-- -- print $ BS.length i.dataArray.rawData
--
-- print fits.primaryHDU.dataArray.rawData
--
-- let out = encode fits
-- BS.writeFile "/Users/seanhess/code/notebooks/data/out.fits" out

-- Quantiies To Fits -------------------------------------------------

data Quantities (as :: [Type]) = Quantities
  { opticalDepth :: Results as
  , temperature :: Results as
  , electronPressure :: Results as
  , microTurbulence :: Results as
  , magStrength :: Results as
  , velocity :: Results as
  , magInclination :: Results as
  , magAzimuth :: Results as
  , geoHeight :: Results as
  , gasPressure :: Results as
  , density :: Results as
  }


quantitiesFits :: Quantities [SlitX, Depth] -> Fits
quantitiesFits q = Fits primaryHDU $ fmap Image $ quantitiesHDUs q


-- What is supposed to go in here?
primaryHDU :: PrimaryHDU
primaryHDU = PrimaryHDU primaryHeaders emptyDataArray
 where
  primaryHeaders =
    Header
      [keyword "WOOT" (String "this is a very long string passing 30 characters") Nothing, keyword "CUSTOM" (String "adf") Nothing]


-- TODO: generate a bunch of HDUs!
quantitiesHDUs :: Quantities [SlitX, Depth] -> [ImageHDU]
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
  dataHDU :: (Writer [ImageHDU] :> es) => Results [SlitX, Depth] -> [HeaderRecord] -> Eff es ()
  dataHDU res h = do
    let header = Header h
        dataArray = encodeArray res.array
    tell [ImageHDU{header, dataArray}]

  dataHeaders :: Text -> Text -> Text -> [HeaderRecord]
  dataHeaders ext bt bu =
    [ extName ext
    , bType bt
    , bUnit bu
    , custom "woot"
    ]

  extName e = keyword "EXTNAME" (String e) Nothing
  bType bt = keyword "BTYPE" (String bt) (Just "Uniform Content Descriptor (UCD)")
  bUnit "" = keyword "BUNIT" (String "") (Just "dimensionless")
  bUnit bu = keyword "BUNIT" (String bu) Nothing
  custom n = keyword "CUSTOM" (String n) Nothing

  opticalDepth =
    dataHDU q.opticalDepth $ dataHeaders "Log of optical depth at 500nm" "phys.absorption.opticalDepth" ""

  temperature =
    dataHDU q.temperature $ dataHeaders "Temperature" "phys.temperature" "K"

  electronPressure =
    dataHDU q.electronPressure $ dataHeaders "Electron Pressure" "phys.electron;phys.pressure" "N/m^2"

  microTurbulence =
    dataHDU q.microTurbulence $ dataHeaders "Microturbulence" "phys.veloc.microTurb" "km/s"

  magStrength =
    dataHDU q.magStrength $ dataHeaders "Magnetic Field Strength" "phys.magField" "T"

  velocity =
    dataHDU q.velocity $ dataHeaders "Line-of-sight Velocity" "spect.dopplerVeloc" "km/s"

  magInclination =
    dataHDU q.magInclination $ dataHeaders "Magnetic Field Inclination (w.r.t. line-of-sight)" "phys.magField;pos.angDistance" "deg"

  magAzimuth =
    dataHDU q.magAzimuth $ dataHeaders "Magnetic Field Azimuth (w.r.t. line-of-sight)" "phys.magField;pos.azimuth" "deg"

  geoHeight =
    dataHDU q.geoHeight $ dataHeaders "Geometric Height above solar surface (tau ~ 1 at 500nm)" "phys.distance" "km"

  gasPressure =
    dataHDU q.gasPressure $ dataHeaders "Gas Pressure" "phys.pressure" "N/m^2"

  density =
    dataHDU q.density $ dataHeaders "Density" "phys.density" "kg/m^3"


-- Parse Quantities ---------------------------------------------------------------------------------

readQuantitiesFrames :: (MonadIO m, MonadThrow m) => FilePath -> m [Quantities [SlitX, Depth]]
readQuantitiesFrames fp = do
  inp <- liftIO $ BS.readFile fp
  res <- decodeResults inp
  resultsQuantities res


decodeResults :: (MonadThrow m) => BS.ByteString -> m (Results [Quantity, Depth, FrameY, SlitX])
decodeResults inp = do
  f <- decode inp
  a <- decodeArray @Ix4 @Float f.primaryHDU.dataArray
  pure $ Results a


resultsQuantities :: (MonadThrow m) => Results [Quantity, Depth, FrameY, SlitX] -> m [Quantities [SlitX, Depth]]
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


splitQuantitiesM :: (MonadThrow m) => Results [Quantity, Depth, SlitX] -> m (Quantities [SlitX, Depth])
splitQuantitiesM rbf =
  case splitQuantities rbf of
    Nothing -> throwM $ InvalidFrameShape (size rbf.array)
    Just qs -> pure qs


splitQuantities :: Results [Quantity, Depth, SlitX] -> Maybe (Quantities [SlitX, Depth])
splitQuantities res = do
  let qs = fmap transpose2 $ outerList res
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


outerList
  :: forall a as
   . (Lower (ResultsIx (a : as)) ~ ResultsIx as, Index (ResultsIx as), Index (ResultsIx (a : as)))
  => Results (a : as)
  -> [Results as]
outerList (Results a) = foldOuterSlice row a
 where
  row :: Array D (ResultsIx as) Float -> [Results as]
  row r = [Results r]


transpose2 :: Results [a, b] -> Results [b, a]
transpose2 (Results arr) = Results $ transposeInner arr


-- Errors ------------------------------------------

data GenerateError
  = InvalidFrameShape (Sz Ix3)
  | InvalidFits String
  | FrameOutOfBounds (Sz Ix4) Int
  deriving (Show, Eq, Exception)
