module NSO.Image.GWCS.L1GWCS where

import NSO.Image.Headers.WCS (X, Y)
import NSO.Prelude hiding (identity)
import NSO.Types.Common (Stokes)
import Telescope.Asdf
import Telescope.Asdf.GWCS as GWCS hiding (parseCompose, parseConcat, parseDirect)
import Telescope.Data.Parser


-- DONE: Parse L1 focused past the spectral, dropping the remappings
--  existing transform is (X, Y, Stokes) -> (HPLon, HPLat, Time, Stokes)
--  so the remaps in L1 are to swap: (X, Wav, Y, Stokes) -> (Wav, X, Y, Stokes)
--  and afterwards (Wav, HPLon, HPLat, Time, Stokes) -> (HPLon, Wav, HPLat, Time, Stokes)
--  yeah, you can tell by the axes_order in the final frame
-- TODO: Manually remap the axes as needed. (X, Wav, Y, Stokes) -> (Wav, X, Y, Stokes)

--  existing transform is (X, Y, Stokes) -> (HPLon, HPLat, Time, Stokes)
--  so the remaps in L1 are to swap: (X, Wav, Y, Stokes) -> (Wav, X, Y, Stokes)
--  and afterwards (Wav, HPLon, HPLat, Time, Stokes) -> (HPLon, Wav, HPLat, Time, Stokes)
--  yeah, you can tell by the axes_order in the final frame
varyingTransform :: (ToAxes (inp Stokes)) => L1WCSTransform -> Transform (Scale X, Pix Y, inp Stokes) (HPLon, HPLat, Time, Stokes)
varyingTransform = transform


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
  -- transform: !transform/compose-1.2.0
  --   forward:
  --   - !transform/compose-1.2.0
  --     forward:
  --     - !transform/remap_axes-1.3.0
  --       inputs: [x0, x1, x2, x3]
  --       mapping: [1, 0, 2, 3]
  --       outputs: [x0, x1, x2, x3]
  --     - !transform/concatenate-1.2.0
  --       forward:
  --       - !transform/linear1d-1.0.0
  --         inputs: [x]
  --         intercept: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 nm,
  --           value: 853.7012736084624}
  --         name: Spectral
  --         outputs: [y]
  --         slope: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 nm.pixel**-1,
  --           value: 0.000999852488051306}
  --       - !<asdf://dkist.nso.edu/tags/coupled_compound_model-1.0.0>
  --         forward:
  parseValue = \case
    Object o -> do
      ss <- o .: "steps"
      case ss of
        [GWCSStep _ (Just t1), GWCSStep f2 Nothing] -> do
          t <- parseAt "[0].transform" $ parseTransformNoSpectral t1
          h <- parseAt "[1].frame" $ parseHelioFrame f2
          pure $ L1GWCS t h
        _ -> expected "l1 gwcs [GWCSStep _ trans, GWCSStep frame ~]" ss
    other -> expected "l1 gwcs" other
   where
    parseHelioFrame :: (Parser :> es) => Object -> Eff es HelioprojectiveFrame
    parseHelioFrame compFrame = do
      compFrames :: [Object] <- compFrame .: "frames"
      case compFrames of
        [_spectral, celest, _temporal, _stokes] -> do
          celest .: "reference_frame"
        other -> expected "final step composite frame [spectral, celest, temporal, stokes]" other

    parseTransformNoSpectral :: (Parser :> es) => Transformation -> Eff es L1WCSTransform
    parseTransformNoSpectral t = do
      (next, _) <- parseCompose t
      (_, ccat) <- parseCompose next
      (_, coupled) <- parseConcat ccat
      n <- parseDirect coupled
      pure $ L1WCSTransform n

    parseCompose :: (Parser :> es) => Transformation -> Eff es (Transformation, Transformation)
    parseCompose t = parseAt "forward" $ do
      case t.forward of
        Compose t1 t2 -> pure (t1, t2)
        other -> expected "Compose" other

    parseConcat :: (Parser :> es) => Transformation -> Eff es (Transformation, Transformation)
    parseConcat t = parseAt "forward" $ do
      case t.forward of
        Concat t1 t2 -> pure (t1, t2)
        other -> expected "Concat" other

    parseDirect :: (Parser :> es) => Transformation -> Eff es Node
    parseDirect t = parseAt "forward" $ do
      case t.forward of
        Direct n -> pure n
        other -> expected "Direct" other


instance ToAsdf L1WCSTransform where
  schema (L1WCSTransform n) = n.schema
  toValue (L1WCSTransform n) = n.value

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
