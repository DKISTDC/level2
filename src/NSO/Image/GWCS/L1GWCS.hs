module NSO.Image.GWCS.L1GWCS where

import NSO.Image.Headers.Types (PixelsPerBin (..))
import NSO.Image.Headers.WCS (Wav, X, Y)
import NSO.Prelude hiding (identity)
import NSO.Types.Common (Stokes)
import Telescope.Asdf
import Telescope.Asdf.GWCS as GWCS
import Telescope.Data.Parser


data HPLat deriving (Generic, ToAxes)
data HPLon deriving (Generic, ToAxes)
data Time deriving (Generic, ToAxes)


newtype L1WCSTransform = L1WCSTransform Node


data L1Asdf = L1Asdf
  { dataset :: L1AsdfDataset
  }
  deriving (Generic, FromAsdf)


data L1AsdfDataset = L1AsdfDataset
  { wcs :: L1GWCS
  }
  deriving (Generic, FromAsdf)


data L1GWCS = L1GWCS
  { transform :: L1WCSTransform
  , helioprojectiveFrame :: HelioprojectiveFrame
  }


data Zero a deriving (Generic, ToAxes)


instance FromAsdf L1GWCS where
  parseValue = \case
    Object o -> do
      -- Parse the steps as objects, not GWCSSteps
      -- , because we want to read their Node directly
      ss <- o .: "steps"
      case ss of
        [Object s1, Object s2] -> do
          n :: Node <- s1 .: "transform"

          compFrame :: Object <- s2 .: "frame"
          compFrames :: [Object] <- compFrame .: "frames"
          case compFrames of
            [_spectral, celest, _temporal, _stokes] -> do
              helio <- celest .: "reference_frame"
              pure $ L1GWCS (L1WCSTransform n) helio
            other -> expected "final step 2 frames" other
        _ -> expected "l1 gwcs [step1, step2]" ss
    other -> expected "l1 gwcs" other


instance ToAsdf L1WCSTransform where
  schema (L1WCSTransform n) = n.schema
  toValue (L1WCSTransform n) = n.value


l1WCSTransform :: L1WCSTransform -> Transform (Scale X, Zero Wav, Pix Y, Zero Stokes) (HPLon, HPLat, Time, Zero Wav, Zero Stokes)
l1WCSTransform t = originalL1Transform |> reorderOutput
 where
  originalL1Transform :: Transform (Scale X, Zero Wav, Pix Y, Zero Stokes) (HPLon, Wav, HPLat, Time, Stokes)
  originalL1Transform = transform t

  reorderOutput :: Transform (HPLon, Wav, HPLat, Time, Stokes) (HPLon, HPLat, Time, Zero Wav, Zero Stokes)
  reorderOutput = transform $ Mapping [0, 2, 3, 1, 4]


-- "fix" inputs to unused input axes in the l1 transform: wavlength and stokes
l1FixInputs :: Transform (Pix X, Pix Y) (Pix X, Pix Wav, Pix Y, Pix Stokes)
l1FixInputs = transform $ Mapping [0, 0, 1, 0]


l1ScaleAxes :: PixelsPerBin -> Transform (Pix X, Pix Wav, Pix Y, Pix Stokes) (Scale X, Zero Wav, Pix Y, Zero Stokes)
l1ScaleAxes (PixelsPerBin p) = binX <&> zero <&> identity @(Pix Y) <&> zero
 where
  binX :: Transform (Pix X) (Scale X)
  binX = scale (fromIntegral p)

  zero :: (ToAxes a) => Transform (Pix a) (Zero a)
  zero = transform $ Scale 0
