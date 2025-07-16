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


varyingTransform :: L1WCSTransform -> Transform (Scale X, Zero Wav, Pix Y, Zero Stokes) (HPLon, HPLat, Time, Zero Wav, Zero Stokes)
varyingTransform t = originalL1Transform |> reorderOutput
 where
  originalL1Transform :: Transform (Scale X, Zero Wav, Pix Y, Zero Stokes) (HPLon, Wav, HPLat, Time, Stokes)
  originalL1Transform = transform t

  reorderOutput :: Transform (HPLon, Wav, HPLat, Time, Stokes) (HPLon, HPLat, Time, Zero Wav, Zero Stokes)
  reorderOutput = transform $ Mapping [0, 2, 3, 1, 4]


scaleZeroAxes :: PixelsPerBin -> Transform (Pix X, Pix Wav, Pix Y, Pix Stokes) (Scale X, Zero Wav, Pix Y, Zero Stokes)
scaleZeroAxes (PixelsPerBin p) = binX <&> zero <&> identity @(Pix Y) <&> zero
 where
  binX :: Transform (Pix X) (Scale X)
  binX = scale (fromIntegral p)

  zero :: (ToAxes a) => Transform (Pix a) (Zero a)
  zero = transform $ Const1D $ Quantity Pixel (Integer 0)


fixInputs :: Transform (Pix X, Pix Y) (Pix X, Pix Wav, Pix Y, Pix Stokes)
fixInputs = transform $ Mapping [0, 0, 1, 0]


data Const1D = Const1D Quantity
instance ToAsdf Const1D where
  schema _ = "!transform/constant-1.2.0"
  toValue (Const1D q) =
    Object
      [ ("dimensions", toNode $ Integer 1)
      , ("value", toNode q)
      ]
instance FromAsdf Const1D where
  parseValue val = do
    o <- parseValue @Object val
    Const1D <$> o .: "value"

-- fixWavStokes0 :: (ToAxes out) => Transform (Pix X, Pix Wav, Pix Y, Pix Stokes) out -> Transform (Pix X, Pix Y) out
-- fixWavStokes0 (Transform t) = transform $ FixInputs t [(1, 0), (3, 0)]
--
--
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
