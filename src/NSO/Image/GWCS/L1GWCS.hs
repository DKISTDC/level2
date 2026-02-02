module NSO.Image.GWCS.L1GWCS where

import Debug.Trace
import NSO.Image.Asdf.Ref
import NSO.Image.Headers.WCS (X, Y)
import NSO.Prelude hiding (identity)
import NSO.Types.Common (Stokes)
import Telescope.Asdf
import Telescope.Asdf.GWCS as GWCS hiding (parseCompose, parseConcat, parseDirect)
import Telescope.Data.KnownText
import Telescope.Data.Parser


type VaryingInput inp = (Scale X, Pix Y, inp Stokes)
type VaryingOutput = (HPLon, HPLat, Time, Stokes)


--  existing transform is (X, Y, Stokes) -> (HPLon, HPLat, Time, Stokes)
--  so the remaps in L1 are to swap: (X, Wav, Y, Stokes) -> (Wav, X, Y, Stokes)
--  and afterwards (Wav, HPLon, HPLat, Time, Stokes) -> (HPLon, Wav, HPLat, Time, Stokes)
--  yeah, you can tell by the axes_order in the final frame
varyingTransform
  :: (ToAxes (inp Stokes))
  => L1WCSTransform
  -> Transform (VaryingInput inp) VaryingOutput
varyingTransform = transform


varyingTransformRef
  :: forall inp
   . (ToAxes (inp Stokes))
  => Transform (VaryingInput inp) VaryingOutput
varyingTransformRef =
  Transform $
    Transformation
      (toAxes @(VaryingInput inp))
      (toAxes @VaryingOutput)
      (Direct $ toNode (Ref @L1WCSTransform))


data HPLat deriving (Generic, ToAxes)
data HPLon deriving (Generic, ToAxes)
data Time deriving (Generic, ToAxes)


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
  , helioFrame :: L1HelioFrame
  }
  deriving (Generic, ToAsdf)


newtype L1WCSTransform = L1WCSTransform {node :: Node}
instance KnownText L1WCSTransform where
  knownText = "L1WCSTransform"


instance ToAsdf L1WCSTransform where
  schema l1 = l1.node.schema
  anchor _ = Just $ Anchor $ knownText @L1WCSTransform
  toNode l1 = Node (schema l1.node) (anchor l1) (toValue l1)


  -- toValue (L1WCSTransform n) = toValue n
  toValue l1 =
    toValue $ l1WCSTransformation l1


l1WCSTransformation :: L1WCSTransform -> Transformation
l1WCSTransformation l1 =
  Transformation
    (toAxes @(VaryingInput Pix))
    (toAxes @VaryingOutput)
    (Direct l1.node)


newtype L1HelioFrame = L1HelioFrame {frame :: HelioprojectiveFrame}


instance KnownText L1HelioFrame where
  knownText = "L1HelioFrame"


instance ToAsdf L1HelioFrame where
  schema (L1HelioFrame h) = schema h
  anchor _ = Just $ Anchor $ knownText @L1HelioFrame
  toValue (L1HelioFrame h) = toValue h


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
          pure $ L1GWCS t $ L1HelioFrame h
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
      node <- parseCoupled coupled
      pure $ L1WCSTransform node

    parseCoupled :: (Parser :> es) => Transformation -> Eff es Node
    parseCoupled f = parseAt "coupled_compound_model" $ do
      ts <- parseDirectForward f
      case ts of
        [comp, _] -> do
          (_, c2) <- parseCompose comp
          (_, v :: Transformation) <- parseCompose c2
          parseDirect v
        other -> expected "coupled" other

    parseCompose :: (Parser :> es) => Transformation -> Eff es (Transformation, Transformation)
    parseCompose t = parseAt "compose" $ do
      case t.forward of
        Compose t1 t2 -> pure (t1, t2)
        other -> expected "Compose" other

    parseConcat :: (Parser :> es) => Transformation -> Eff es (Transformation, Transformation)
    parseConcat t = parseAt "concat" $ do
      case t.forward of
        Concat t1 t2 -> pure (t1, t2)
        other -> expected "Concat" other

    -- Direct gives me the entire Node (schema) _ (parent node)
    parseDirect :: (Parser :> es) => Transformation -> Eff es Node
    parseDirect t = parseAt "Direct" $ do
      case t.forward of
        Direct n -> pure n
        other -> expected "Direct" other

    parseDirectForward :: (Parser :> es) => Transformation -> Eff es [Transformation]
    parseDirectForward t = do
      n <- parseDirect t
      case n.value of
        Object o -> do
          o .: "forward"
        val -> expected "Object" val

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
