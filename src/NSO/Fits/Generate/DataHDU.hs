module NSO.Fits.Generate.DataHDU where

import Effectful
import Effectful.Writer.Static.Local
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Doc as Doc
import NSO.Fits.Generate.Frames
import NSO.Fits.Generate.Headers
import NSO.Fits.Generate.Keywords
import NSO.Fits.Generate.Types
import NSO.Prelude
import Telescope.Fits
import Telescope.Fits.Types (HeaderRecord (..))


type OpticalDepth = DataHDUInfo "Log of Optical Depth at 500nm" 'OpticalDepth 'Dimensionless
type Temperature = DataHDUInfo "Temperature" 'Temperature 'Kelvin
type ElectronPressure = DataHDUInfo "ElectronPressure" 'ElectronPressure 'Kelvin


-- data DataHDU info = DataHDU
--   { info :: info
--   , -- this isn't in quite the right shape. I need to get it into dummyY, etc
--     dataArray :: Results Frame
--   }

data DataHDUHeader info = DataHDUHeader info


data DataHDUInfo (extName :: Symbol) (ucd :: UCD) (unit :: Unit) = DataHDUInfo


data DataHDUAxes = DataHDUAxes
  { naxis :: Key (Constant "3") "Data HDUs have the shape (y, x, depth)"
  , naxis1 :: Naxis "Optical Depth"
  , naxis2 :: Naxis "Slit X"
  , naxis3 :: NaxisY
  }
  deriving (Generic, HeaderDoc)


instance (KnownSymbol ext, KnownValue btype, KnownValue bunit) => HeaderKeywords (DataHDUInfo ext btype bunit) where
  headerKeywords _ =
    [ keywordRecord @(ExtName ext) ExtName
    , keywordRecord @(BType btype) BType
    , keywordRecord @(BUnit bunit) BUnit
    ]


instance (HeaderKeywords info) => HeaderKeywords (DataHDUHeader info) where
  headerKeywords (DataHDUHeader info) =
    headerKeywords @info info


instance (KnownSymbol ext, KnownValue btype, KnownValue bunit) => HeaderDoc (DataHDUInfo ext btype bunit) where
  headerDoc =
    [ docKey @(ExtName ext)
    , docKey @(BType btype)
    , docKey @(BUnit bunit)
    ]


-- instance (HeaderDoc info) => HeaderDoc (DataHDUHeader info) where
--   headerDoc =
--     headerDoc @info
--       <> (headerDoc @DataHDUAxes)
--       <> (headerDoc @PrimaryHeader)

quantitiesHDUs :: Quantities [SlitX, Depth] -> [ImageHDU]
quantitiesHDUs q = runPureEff . execWriter $ do
  opticalDepth
  temperature
  electronPressure
 where
  -- microTurbulence
  -- magStrength
  -- velocity
  -- magInclination
  -- magAzimuth
  -- geoHeight
  -- gasPressure
  -- density

  -- how do you know which is which?
  dataHDU :: forall info es. (HeaderKeywords info, Writer [ImageHDU] :> es) => info -> Results Frame -> Eff es ()
  dataHDU info res = do
    let keywords = headerKeywords @(DataHDUHeader info) (DataHDUHeader info)
        header = Header $ fmap Keyword keywords
        dataArray = encodeArray res.array
    tell [ImageHDU{header, dataArray}]

  -- dataHeaders :: Text -> Text -> Text -> [HeaderRecord]
  -- dataHeaders ext bt bu =
  --   [ extName ext
  --   , bType bt
  --   , bUnit bu
  --   , custom "woot"
  --   ]

  -- extName e = keyword "EXTNAME" (String e) Nothing
  -- bType bt = keyword "BTYPE" (String bt) (Just "Uniform Content Descriptor (UCD)")
  -- bUnit "" = keyword "BUNIT" (String "") (Just "dimensionless")
  -- bUnit bu = keyword "BUNIT" (String bu) Nothing
  -- custom n = keyword "CUSTOM" (String n) Nothing

  opticalDepth =
    dataHDU @OpticalDepth DataHDUInfo q.opticalDepth

  temperature =
    dataHDU @Temperature DataHDUInfo q.temperature

  electronPressure =
    dataHDU @ElectronPressure DataHDUInfo q.electronPressure

-- microTurbulence =
--   dataHDU q.microTurbulence $ dataHeaders "Microturbulence" "phys.veloc.microTurb" "km/s"
--
-- magStrength =
--   dataHDU q.magStrength $ dataHeaders "Magnetic Field Strength" "phys.magField" "T"
--
-- velocity =
--   dataHDU q.velocity $ dataHeaders "Line-of-sight Velocity" "spect.dopplerVeloc" "km/s"
--
-- magInclination =
--   dataHDU q.magInclination $ dataHeaders "Magnetic Field Inclination (w.r.t. line-of-sight)" "phys.magField;pos.angDistance" "deg"
--
-- magAzimuth =
--   dataHDU q.magAzimuth $ dataHeaders "Magnetic Field Azimuth (w.r.t. line-of-sight)" "phys.magField;pos.azimuth" "deg"
--
-- geoHeight =
--   dataHDU q.geoHeight $ dataHeaders "Geometric Height above solar surface (tau ~ 1 at 500nm)" "phys.distance" "km"
--
-- gasPressure =
--   dataHDU q.gasPressure $ dataHeaders "Gas Pressure" "phys.pressure" "N/m^2"
--
-- density =
-- dataHDU q.density $ dataHeaders "Density" "phys.density" "kg/m^3"
