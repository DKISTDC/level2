{-# LANGUAGE UndecidableInstances #-}

module NSO.Fits.Generate where

-- import Data.Massiv.Array

import Data.ByteString qualified as BS
import NSO.Fits.Generate.DataHDU (quantitiesHDUs)
import NSO.Fits.Generate.Frames
import NSO.Fits.Generate.Types
import NSO.Prelude
import Telescope.Fits


------------------------------------------------------------------------------

testInput :: FilePath
testInput = "/Users/seanhess/Data/scan1807/inv_res_mod.fits"


test :: IO ()
test = do
  putStrLn "TEST"
  (f : _) <- readQuantitiesFrames testInput
  let hdus = quantitiesHDUs f
  let (od : qs) = hdus

  print od.header

  -- print $ size f.temperature.array

  -- print $ f.temperature.array !> 1

  -- let dat = encodeArray f.opticalDepth.array
  -- print dat.axes
  -- print dat.bitpix
  -- print $ BS.length dat.rawData
  --
  let fits = quantitiesFits f
  print $ length fits.extensions

  --
  -- let (Image i : _) = fits.extensions
  -- -- print $ BS.length i.dataArray.rawData
  --
  -- print fits.primaryHDU.dataArray.rawData

  --
  let out = encode fits
  BS.writeFile "/Users/seanhess/code/notebooks/data/out.fits" out


--

-- TODO: add primary HDU somewhere ...
quantitiesFits :: Quantities [SlitX, Depth] -> Fits
quantitiesFits q = Fits primaryHDU $ fmap Image $ quantitiesHDUs q


-- What is supposed to go in here?
primaryHDU :: PrimaryHDU
primaryHDU = PrimaryHDU primaryHeaders emptyDataArray
 where
  primaryHeaders =
    Header
      [keyword "WOOT" (String "this is a very long string passing 30 characters") Nothing, keyword "CUSTOM" (String "adf") Nothing]
