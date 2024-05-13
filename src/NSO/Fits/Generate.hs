{-# LANGUAGE UndecidableInstances #-}

module NSO.Fits.Generate where

import Data.ByteString qualified as BS
import Data.Massiv.Array
import Data.Time.Clock (getCurrentTime)
import Effectful
import Effectful.Error.Static
import Effectful.GenRandom
import NSO.Fits.Generate.DimArray
import NSO.Fits.Generate.Headers
import NSO.Fits.Generate.Headers.Keywords (HeaderKeywords (..))
import NSO.Fits.Generate.Headers.LiftL1 (LiftL1Error (..))
import NSO.Fits.Generate.Headers.Types (Depth, SlitX)
import NSO.Fits.Generate.Profile
import NSO.Fits.Generate.Quantities (Quantities (..), decodeQuantitiesFrames, quantitiesHDUs)
import NSO.Prelude
import NSO.Types.Common (Id (..))
import NSO.Types.Inversion (Inversion)
import Telescope.Fits


-- DOING: Profile HDUs
--   DONE: Split wavelengths
--   DONE: Design headers for profile HDUs
--   TODO: Make sure axes are good: CRPIX / CRVAL needs to be calculated accurately

------------------------------------------------------------------------------

testInput :: FilePath
testInput = "/Users/seanhess/Data/scan1807/inv_res_mod.fits"


testOriginalProfile :: FilePath
testOriginalProfile = "/Users/seanhess/Data/scan1807/per_ori.fits"


testResultProfile :: FilePath
testResultProfile = "/Users/seanhess/Data/scan1807/inv_res_pre.fits"


-- WARNING: We are working with pid_1_118, but the headers are wrong there
level1Input :: FilePath
level1Input = "/Users/seanhess/Data/pid_1_118/BVJVO/VISP_2022_06_02T22_13_41_664_00630205_I_BVJVO_L1.fits"


-- level1Input :: FilePath
-- level1Input = "/Users/seanhess/Data/pid_2_114/ADDMM/VISP_2023_10_16T23_55_59_513_00589600_I_ADDMM_L1.fits"

test :: IO ()
test = do
  putStrLn "TEST"
  (f : _) <- decodeQuantitiesFrames =<< BS.readFile testInput
  i1 <- readLevel1 level1Input

  pos <- decodeProfileFrames @Original =<< BS.readFile testOriginalProfile

  (po : _) <- pure pos.frames :: IO [ProfileFrame Original]
  print $ size po.wav630.array
  print $ size po.wav854.array

  -- we need to calculate the exact axis of the wavelengths...
  pfs <- decodeProfileFrames @Fit =<< BS.readFile testResultProfile
  (pf : _) <- pure pfs.frames :: IO [ProfileFrame Fit]
  print $ size pf.wav630.array
  print $ size pf.wav854.array

  -- print ("EQUAL FRAMES", length fs, length pos, length pfs)

  -- (frp : _) <- decodeProfileFrames =<< BS.readFile testResultProfile
  -- print $ size frp.array
  --
  -- print $ size oriProfile.array
  -- print $ size resProfile.array

  -- putStrLn "\nCHECK OPTICAL DEPTH"
  -- print $ size f.opticalDepth.array
  -- print $ f.opticalDepth.array !> 0
  -- print $ f.opticalDepth.array !> 20
  -- print $ f.opticalDepth.array !> 40
  --
  -- let hdus = quantitiesHDUs i1.header f
  -- let (od : _) = hdus
  --
  -- putStrLn "\nCHECK HDUS"
  -- print od.header
  -- print od.dataArray.axes

  -- print $ size f.temperature.array

  -- print $ f.temperature.array !> 1

  -- let dat = encodeArray f.opticalDepth.array
  -- print dat.axes
  -- print dat.bitpix
  -- print $ BS.length dat.rawData

  now <- getCurrentTime
  fits <- runGenTestIO $ generateL2Fits now (Id "inv.TEST0") i1 f pos.wavProfiles pfs.wavProfiles po pf

  -- print $ length fits.extensions
  -- let (Image e : _) = fits.extensions
  -- print $ e.header

  -- let (Image i : _) = fits.extensions
  -- -- print $ BS.length i.dataArray.rawData
  --
  -- print fits.primaryHDU.dataArray.rawData

  let out = encode fits
      path = "code/notebooks/data/out.fits"
  BS.writeFile ("/Users/seanhess/" <> path) out
  putStrLn $ "\nWROTE : " <> path


readLevel1 :: (MonadThrow m, MonadIO m) => FilePath -> m BinTableHDU
readLevel1 fp = do
  inp <- liftIO $ BS.readFile fp
  fits <- decode inp
  case fits.extensions of
    [BinTable b] -> pure b
    _ -> throwM $ MissingL1HDU fp


generateL2Fits
  :: (Error LiftL1Error :> es, GenRandom :> es)
  => UTCTime
  -> Id Inversion
  -> BinTableHDU
  -> Quantities [SlitX, Depth]
  -> WavProfiles Original
  -> WavProfiles Fit
  -> ProfileFrame Original
  -> ProfileFrame Fit
  -> Eff es Fits
generateL2Fits now i l1 q wpo wpf po pf = do
  prim <- primaryHDU i l1
  imgs <- quantitiesHDUs now l1.header q
  profs <- profileHDUs now l1.header wpo wpf po pf
  pure $ Fits prim $ fmap Image $ imgs <> profs


-- What is supposed to go in here?
primaryHDU :: (Error LiftL1Error :> es, GenRandom :> es) => Id Inversion -> BinTableHDU -> Eff es PrimaryHDU
primaryHDU di l1 = do
  hs <- writeHeader allKeys
  pure $ PrimaryHDU (Header hs) emptyDataArray
 where
  allKeys = do
    primKeys
    teleKeys
    datacenterKeys
    dkistKeys
    adaptiveKeys

  primKeys = do
    ob <- observationHeader l1.header
    sectionHeader "Observation" "Keys describing the observation and general metadata"
    addKeywords $ headerKeywords @ObservationHeader ob

  teleKeys = do
    th <- telescopeHeader l1.header
    sectionHeader "Telescope" "Keys describing the pointing and op of the Telescope"
    addKeywords $ headerKeywords @TelescopeHeader th

  datacenterKeys = do
    dc <- datacenterHeader l1.header di
    cep <- contribExpProp l1.header
    sectionHeader "Datacenter" "Keys generated by the DKIST data center to describe processing performed, archiving or extra metadata"
    addKeywords $ headerKeywords @Datacenter dc
    addKeywords $ headerKeywords @ContribExpProp cep

  dkistKeys = do
    dk <- dkistHeader l1.header
    sectionHeader "DKIST Operations" "Information about this configuration or operations of the facility when generating this data"
    addKeywords $ headerKeywords @DKISTHeader dk

  adaptiveKeys = do
    ao <- adaptiveOpticsHeader l1.header
    sectionHeader "Adaptive Optics" "Keys describing aspects of the adaptive optics system"
    addKeywords $ headerKeywords @AdaptiveOptics ao


runGenTestIO :: Eff '[GenRandom, Error LiftL1Error, IOE] a -> IO a
runGenTestIO eff = do
  res <- runEff $ runErrorNoCallStack $ runGenRandom eff
  case res of
    Left e -> throwM e
    Right a -> pure a
