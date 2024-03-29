module NSO.Data.Generate where

import Data.ByteString.Lazy qualified as BL
import Data.Fits
import Data.Fits.Array
import Data.Fits.Read
import Data.Massiv.Array as M
import NSO.Prelude


test :: IO ()
test = do
  inp <- BL.readFile "/Users/seanhess/Data/scan1807/inv_res_mod.fits"
  a <- eitherFail $ decodeFile inp
  print $ size a
  print $ size $ a !> 0
  print $ size $ a !> 0 !> 0
  putStrLn "HELLO"


decodeFile :: BL.ByteString -> Either String (Array P Ix4 Float)
decodeFile inp = do
  hdu <- readPrimaryHDU inp
  arr <- decodeImage @Ix4 @Float hdu
  pure arr
