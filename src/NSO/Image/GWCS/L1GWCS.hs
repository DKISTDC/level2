module NSO.Image.GWCS.L1GWCS where

import NSO.Image.Headers.Types (PixelsPerBin (..))
import NSO.Image.Headers.WCS (Wav, X, Y)
import NSO.Prelude hiding (identity)
import NSO.Types.Common (Stokes)
import Telescope.Asdf
import Telescope.Asdf.Core (Quantity (..), Unit (..))
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


varyingTransform :: (ToAxes (inp Wav), ToAxes (inp Stokes)) => L1WCSTransform -> Transform (Scale X, inp Wav, Pix Y, inp Stokes) (HPLon, Wav, HPLat, Time, Stokes)
varyingTransform = transform


-- fixWavStokes0 :: (ToAxes out) => Transform (Pix X, Pix Wav, Pix Y, Pix Stokes) out -> Transform (Pix X, Pix Y) out
-- fixWavStokes0 (Transform t) = transform $ FixInputs t [(1, 0), (3, 0)]

-- -- This ONLY works with integers for some reason (doesn't like quantities)
-- -- combine with Const1D or Scale or something to convert it to a quantity?
-- data FixInputs = FixInputs Transformation [(Int, Int)]
-- instance ToAsdf FixInputs where
--   schema _ = "!transform/fix_inputs-1.2.0"
--   toValue (FixInputs t fs) =
--     Object [("forward", toNode [toNode t, toNode forward])]
--    where
--     forward =
--       Object
--         [ ("keys", toNode $ fmap fst fs)
--         , ("values", toNode $ fmap snd fs)
--         ]
