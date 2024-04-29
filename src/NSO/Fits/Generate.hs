{-# LANGUAGE UndecidableInstances #-}

module NSO.Fits.Generate where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.ByteString qualified as BS
import Data.Massiv.Array
import Effectful
import Effectful.Error.Static
import NSO.Fits.Generate.DataHDU (quantitiesHDUs)
import NSO.Fits.Generate.Frames
import NSO.Fits.Generate.Headers
import NSO.Fits.Generate.Keywords
import NSO.Fits.Generate.Types
import NSO.Prelude
import NSO.Types.Common (Id (..))
import NSO.Types.Inversion (Inversion)
import Telescope.Fits
import Telescope.Fits.Types (HeaderRecord (..))


-- DONE: VerifyWarning: Found a SIMPLE card but its format doesn't respect the FITS Standard [astropy.io.fits.hdu.hdulist]
-- DONE: WCS (CRPIX, CRVAL, CUNIT, CTYPE, etc)

------------------------------------------------------------------------------

testInput :: FilePath
testInput = "/Users/seanhess/Data/scan1807/inv_res_mod.fits"


level1Input :: FilePath
level1Input = "/Users/seanhess/Data/pid_2_114/ADDMM/VISP_2023_10_16T23_55_59_513_00589600_I_ADDMM_L1.fits"


test :: IO ()
test = do
  putStrLn "TEST"
  (f : _) <- readQuantitiesFrames testInput
  i1 <- readLevel1 level1Input

  putStrLn "\nCHECK OPTICAL DEPTH"
  print $ size f.opticalDepth.array
  print $ f.opticalDepth.array !> 0
  print $ f.opticalDepth.array !> 20
  print $ f.opticalDepth.array !> 40

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
  --
  fits <- runGenTestIO $ quantitiesFits (Id "inv.TEST0") i1 f
  print $ length fits.extensions
  let (Image e : _) = fits.extensions
  print $ e.header

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


-- TODO: add primary HDU somewhere ...
quantitiesFits :: (Error FitsGenError :> es) => Id Inversion -> BinTableHDU -> Quantities [SlitX, Depth] -> Eff es Fits
quantitiesFits i l1 q = do
  prim <- primaryHDU i l1
  imgs <- quantitiesHDUs l1.header q
  pure $ Fits prim $ fmap Image imgs


-- What is supposed to go in here?
primaryHDU :: (Error FitsGenError :> es) => Id Inversion -> BinTableHDU -> Eff es PrimaryHDU
primaryHDU di l1 = do
  hs <- writeHeader allKeys
  pure $ PrimaryHDU (Header hs) emptyDataArray
 where
  allKeys = do
    ph <- primaryHeader l1.header di
    th <- telescopeHeader l1.header
    primKeys ph
    teleKeys th

  primKeys ph = do
    sectionHeader "Primary Info" "Primary info goes here"
    addKeywords $ headerKeywords @PrimaryHeader ph

  teleKeys th = do
    sectionHeader "Telescope" "Keys describing the pointing and op of the Telescope"
    addKeywords $ headerKeywords @TelescopeHeader th


runGenTestIO :: Eff '[Error FitsGenError, IOE] a -> IO a
runGenTestIO eff = do
  res <- runEff $ runErrorNoCallStack eff
  case res of
    Left e -> throwM e
    Right a -> pure a
