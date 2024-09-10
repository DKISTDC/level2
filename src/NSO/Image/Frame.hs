{-# LANGUAGE UndecidableInstances #-}

module NSO.Image.Frame where

import Data.ByteString qualified as BS
import Data.Massiv.Array ()
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.GenRandom
import NSO.Image.Headers
import NSO.Image.Headers.Types (DateTime (..), Depth, Key (..), SliceXY, SlitX)
import NSO.Image.Primary
import NSO.Image.Profile
import NSO.Image.Quantities (Quantities (..), Quantity, QuantityError, QuantityHeader, QuantityImage, quantities, quantityHDUs, quantityHeaders)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Inversion (Inversion)
import Telescope.Asdf as Asdf
import Telescope.Fits as Fits
import Telescope.Fits.Encoding (replaceKeywordLine)


data L2Frame = L2Frame
  { primary :: PrimaryHeader
  , quantities :: Quantities Quantity
  , profiles :: Profiles Profile
  }
  deriving (Generic)


instance ToAsdf L2Frame where
  toValue _ =
    Object
      [ ("primary", fromValue Null)
      , ("quantities", fromValue Null)
      , ("profiles", fromValue Null)
      ]


data L2FrameMeta = L2FrameMeta
  { primary :: PrimaryHeader
  , quantities :: Quantities QuantityHeader
  , profiles :: Profiles ProfileHeader
  , path :: Path L2Frame
  }


data L2FrameInputs = L2FrameInputs
  { quantities :: Quantities (QuantityImage [SlitX, Depth])
  , profileFit :: ProfileFrame Fit
  , profileOrig :: ProfileFrame Original
  , l1Frame :: BinTableHDU
  }


filenameL2Frame :: Id Inversion -> DateTime -> Path' Filename L2Frame
filenameL2Frame ii (DateTime dt) = Path $ cs (T.toUpper $ T.map toUnderscore $ ii.fromId <> "_" <> dt) <> "_L2.fits"
 where
  toUnderscore :: Char -> Char
  toUnderscore '.' = '_'
  toUnderscore ':' = '_'
  toUnderscore '-' = '_'
  toUnderscore c = c


generateL2Frame
  :: (Error QuantityError :> es, Error ProfileError :> es, Error PrimaryError :> es, GenRandom :> es)
  => UTCTime
  -> Id Inversion
  -> SliceXY
  -> WavProfiles Original
  -> WavProfiles Fit
  -> L2FrameInputs
  -> Eff es (L2Frame, DateTime)
generateL2Frame now i slice wpo wpf gf = do
  ph <- primaryHeader i gf.l1Frame
  qs <- quantities slice now gf.l1Frame.header gf.quantities
  ps <- profiles slice now gf.l1Frame.header wpo wpf gf.profileOrig gf.profileFit
  let dateBeg = ph.observation.dateBeg.ktype
  let frame = L2Frame{primary = ph, quantities = qs, profiles = ps}
  pure (frame, dateBeg)


frameMeta :: L2Frame -> Path L2Frame -> L2FrameMeta
frameMeta frame path =
  L2FrameMeta
    { primary = frame.primary
    , quantities = quantityHeaders frame.quantities
    , profiles = profileHeaders frame.profiles
    , path
    }


frameToFits :: L2Frame -> Fits
frameToFits frame =
  let prim = primaryHDU frame.primary
      images = quantityHDUs frame.quantities
      profs = profileHDUs frame.profiles
   in Fits prim $ fmap Image (images <> profs)


-- | Encode and insert framevol
encodeL2 :: Fits -> BS.ByteString
encodeL2 f' =
  let out = Fits.encode f'
      mb = fromIntegral (BS.length out) / 1000000
   in replaceKeywordLine "FRAMEVOL" (Float mb) (Just "[Mb]") out


encodeL2Asdf :: (Error AsdfError :> es, IOE :> es) => L2Frame -> Eff es BS.ByteString
encodeL2Asdf = Asdf.encode
