{-# LANGUAGE UndecidableInstances #-}

module NSO.Fits.Generate where

import App.Effect.Scratch as Scratch
import App.Globus (InvProfile, InvResults, OrigProfile)
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Massiv.Array ()
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.FileSystem as FS
import Effectful.GenRandom
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Writer.Static.Local
import NSO.Fits.Generate.Error
import NSO.Fits.Generate.FetchL1 as Fetch (L1Frame)
import NSO.Fits.Generate.Headers
import NSO.Fits.Generate.Headers.Keywords (HeaderKeywords (..))
import NSO.Fits.Generate.Headers.LiftL1 (LiftL1Error (..))
import NSO.Fits.Generate.Headers.Types (DateTime (..), Depth, Key (..), SlitX)
import NSO.Fits.Generate.Profile
import NSO.Fits.Generate.Quantities (Quantities (..), decodeQuantitiesFrames, quantitiesHDUs)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Inversion (Inversion)
import Telescope.Fits as Fits
import Telescope.Fits.Encoding (replaceKeywordLine)


-- import Data.Time.Clock (getCurrentTime)
-- import NSO.Fits.Generate.DataCube

-- DONE: Profile HDUs
--   DONE: Split wavelengths
--   DONE: Design headers for profile HDUs
--   DONE: Make sure axes are good: CRPIX / CRVAL needs to be calculated accurately
-- DONE: Convert units

-- TODO: Cleanup
--   TODO: telescope - change exports to avoid fits-parse
--   DONE: pubmit PR for fits-parse
--   TODO: Refactor primary to a separate gen file from Generate and export as required

------------------------------------------------------------------------------

testInput :: FilePath
testInput = "/Users/seanhess/Data/scan1807/inv_res_mod.fits"


testOriginalProfile :: FilePath
testOriginalProfile = "/Users/seanhess/Data/scan1807/per_ori.fits"


testResultProfile :: FilePath
testResultProfile = "/Users/seanhess/Data/scan1807/inv_res_pre.fits"


-- WARNING: We are working with pid_1_118, but the headers are wrong there
-- level1Input :: FilePath
-- level1Input = "/Users/seanhess/Data/pid_1_118/BVJVO/VISP_2022_06_02T22_13_41_664_00630205_I_BVJVO_L1.fits"

level1Input :: Path L1Frame
level1Input = Path "/Users/seanhess/Data/pid_2_114/ADDMM/VISP_2023_10_16T23_55_59_513_00589600_I_ADDMM_L1.fits"


-- test :: IO ()
-- test = do
--   let ip = Id "id.118958.452436"
--   print ip
--   let tok = "Ag6XJzONoxwB7w7KkgyK8K2pOM3j6V5nrq2jJw44b8Ka6QnmxMh2Cyyne49KX3QpMK7neq896Kp4zpF7J00X1FDQV8v" :: Token Access
--   md <- findCanonicalDataset ip
--   print md
--

readQuantitiesFrames :: (Scratch :> es) => Path InvResults -> Eff es [Quantities [SlitX, Depth]]
readQuantitiesFrames p = do
  inp <- send $ Scratch.ReadFile p
  decodeQuantitiesFrames inp


readOrigProfileFrames :: (Scratch :> es) => Path OrigProfile -> Eff es (ProfileFrames Original)
readOrigProfileFrames p = do
  inp <- send $ Scratch.ReadFile p
  decodeProfileFrames @Original inp


readFitProfileFrames :: (Scratch :> es) => Path InvProfile -> Eff es (ProfileFrames Fit)
readFitProfileFrames p = do
  inp <- send $ Scratch.ReadFile p
  decodeProfileFrames @Fit inp


data GenerateFrame = GenerateFrame
  { quantities :: Quantities [SlitX, Depth]
  , profileFit :: ProfileFrame Fit
  , profileOrig :: ProfileFrame Original
  , l1Frame :: BinTableHDU
  }


collateFrames :: (Error GenerateError :> es) => [Quantities [SlitX, Depth]] -> [ProfileFrame Fit] -> [ProfileFrame Original] -> [BinTableHDU] -> Eff es [GenerateFrame]
collateFrames qs pfs pos ts
  | allFramesEqual = pure $ L.zipWith4 GenerateFrame qs pfs pos ts
  | otherwise = throwError mismatchError
 where
  allFramesEqual :: Bool
  allFramesEqual =
    all (== length qs) frameSizes

  frameSizes :: [Int]
  frameSizes = [length qs, length pfs, length pos, length ts]

  mismatchError :: GenerateError
  mismatchError = MismatchedFrames frameSizes


writeL2Frame :: (Log :> es, Scratch :> es, FileSystem :> es) => Id Inversion -> Fits -> DateTime -> Eff es ()
writeL2Frame ii f (DateTime dt) = do
  let dir = outputL2 ii
  let path = filePath dir filenameL2
  send $ Scratch.WriteFile path $ Fits.encode f
 where
  filenameL2 :: Path' Filename L2Frame
  filenameL2 = Path $ cs (T.toUpper $ T.map toUnderscore $ ii.fromId <> "_" <> dt) <> "_L2.fits"

  toUnderscore :: Char -> Char
  toUnderscore '.' = '_'
  toUnderscore ':' = '_'
  toUnderscore '-' = '_'
  toUnderscore c = c


-- testOld :: IO ()
-- testOld = do
--   putStrLn "TEST"
--   (f : _) <- decodeQuantitiesFrames =<< BS.readFile testInput
--   i1 <- runEff $ runFileSystem $ Fetch.readLevel1File level1Input
--
--   pos <- decodeProfileFrames @Original =<< BS.readFile testOriginalProfile
--
--   (po : _) <- pure pos.frames :: IO [ProfileFrame Original]
--   print $ size po.wav630.array
--   print $ size po.wav854.array
--
--   -- we need to calculate the exact axis of the wavelengths...
--   pfs <- decodeProfileFrames @Fit =<< BS.readFile testResultProfile
--   (pf : _) <- pure pfs.frames :: IO [ProfileFrame Fit]
--   print $ size pf.wav630.array
--   print $ size pf.wav854.array
--
--   -- print ("EQUAL FRAMES", length fs, length pos, length pfs)
--
--   -- (frp : _) <- decodeProfileFrames =<< BS.readFile testResultProfile
--   -- print $ size frp.array
--   --
--   -- print $ size oriProfile.array
--   -- print $ size resProfile.array
--
--   -- putStrLn "\nCHECK OPTICAL DEPTH"
--   -- print $ size f.opticalDepth.array
--   -- print $ f.opticalDepth.array !> 0
--   -- print $ f.opticalDepth.array !> 20
--   -- print $ f.opticalDepth.array !> 40
--   --
--   -- let hdus = quantitiesHDUs i1.header f
--   -- let (od : _) = hdus
--   --
--   -- putStrLn "\nCHECK HDUS"
--   -- print od.header
--   -- print od.dataArray.axes
--
--   -- print $ size f.temperature.array
--
--   -- print $ f.temperature.array !> 1
--
--   -- let dat = encodeArray f.opticalDepth.array
--   -- print dat.axes
--   -- print dat.bitpix
--   -- print $ BS.length dat.rawData
--
--   let gen = GenerateFrame f pf po i1
--   now <- getCurrentTime
--   fits <- runGenTestIO $ generateL2Fits now (Id "inv.TEST0") pos.wavProfiles pfs.wavProfiles gen
--
--   -- print $ length fits.extensions
--   -- let (Image e : _) = fits.extensions
--   -- print $ e.header
--
--   -- let (Image i : _) = fits.extensions
--   -- -- print $ BS.length i.dataArray.rawData
--   --
--   -- print fits.primaryHDU.dataArray.rawData
--
--   let out = encodeL2 fits
--       path = "code/notebooks/data/out.fits"
--
--   print $ BS.drop (BS.length out - 100) out
--   BS.writeFile ("/Users/seanhess/" <> path) out
--   putStrLn $ "\nWROTE : " <> path

encodeL2 :: Fits -> BS.ByteString
encodeL2 f =
  let out = encode f
      mb = fromIntegral (BS.length out) / 1000000
   in replaceKeywordLine "FRAMEVOL" (Float mb) (Just "[Mb]") out


generateL2Fits
  :: (Error GenerateError :> es, GenRandom :> es)
  => UTCTime
  -> Id Inversion
  -> WavProfiles Original
  -> WavProfiles Fit
  -> GenerateFrame
  -> Eff es (Fits, DateTime)
generateL2Fits now i wpo wpf gf =
  runErrorNoCallStackWith @LiftL1Error (throwError . LiftL1) $ do
    (prim, dateBeg) <- primaryHDU i gf.l1Frame
    imgs <- quantitiesHDUs now gf.l1Frame.header gf.quantities
    profs <- profileHDUs now gf.l1Frame.header wpo wpf gf.profileOrig gf.profileFit
    let fits = Fits prim $ fmap Image $ imgs <> profs
    pure (fits, dateBeg)


-- What is supposed to go in here?
primaryHDU :: (Error LiftL1Error :> es, GenRandom :> es) => Id Inversion -> BinTableHDU -> Eff es (PrimaryHDU, DateTime)
primaryHDU di l1 = do
  (dateBeg, hs) <- runWriter allKeys
  let hdu = PrimaryHDU (Header hs) emptyDataArray
  pure (hdu, dateBeg)
 where
  allKeys = do
    dateBeg <- primKeys
    teleKeys
    datacenterKeys
    dkistKeys
    adaptiveKeys
    pure dateBeg

  primKeys = do
    ob <- observationHeader l1.header
    sectionHeader "Observation" "Keys describing the observation and general metadata"
    addKeywords $ headerKeywords @ObservationHeader ob
    pure ob.dateBeg.ktype

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

-- runGenTestIO :: Eff '[GenRandom, Error LiftL1Error, IOE] a -> IO a
-- runGenTestIO eff = do
--   res <- runEff $ runErrorNoCallStack $ runGenRandom eff
--   case res of
--     Left e -> throwM e
--     Right a -> pure a
