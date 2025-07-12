module NSO.Image.GWCS.L1Transform where

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
  }


data Zero a deriving (Generic, ToAxes)


instance FromAsdf L1GWCS where
  parseValue = \case
    Object o -> do
      ss :: [Value] <- o .: "steps"
      case ss of
        [Object s1, _] -> do
          n :: Node <- s1 .: "transform"
          pure $ L1GWCS $ L1WCSTransform n
        _ -> expected "l1 gwcs [step1, step2]" ss
    other -> expected "l1 gwcs" other


instance ToAsdf L1WCSTransform where
  schema (L1WCSTransform n) = n.schema
  toValue (L1WCSTransform n) = n.value


l1WCSTransform :: L1WCSTransform -> Transform (Pix X, Pix Y) (HPLon, HPLat, Time, Zero Wav, Zero Stokes)
l1WCSTransform t = fixL1Inputs |> zeroWavStokes |> originalL1Transform |> reorderOutput
 where
  originalL1Transform :: Transform (Pix X, Zero Wav, Pix Y, Zero Stokes) (HPLon, Wav, HPLat, Time, Stokes)
  originalL1Transform = transform t

  fixL1Inputs :: Transform (Pix X, Pix Y) (Pix X, Pix Wav, Pix Y, Pix Stokes)
  fixL1Inputs = transform $ Mapping [0, 0, 1, 0]

  zero :: (ToAxes a) => Transform (Pix a) (Zero a)
  zero = transform $ Scale 0

  zeroWavStokes :: Transform (Pix X, Pix Wav, Pix Y, Pix Stokes) (Pix X, Zero Wav, Pix Y, Zero Stokes)
  zeroWavStokes = identity @(Pix X) <&> zero <&> identity @(Pix Y) <&> zero

  reorderOutput :: Transform (HPLon, Wav, HPLat, Time, Stokes) (HPLon, HPLat, Time, Zero Wav, Zero Stokes)
  reorderOutput = transform $ Mapping [0, 2, 3, 1, 4]
