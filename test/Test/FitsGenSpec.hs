module Test.FitsGenSpec where

import App.Worker.Generate (GenerateError, runGenerateError)
import Control.Monad.Catch (throwM)
import Data.ByteString qualified as BS
import Data.Massiv.Array as M (Comp (..), P)
import Data.Massiv.Array qualified as M
import Data.Time.Clock (getCurrentTime)
import Effectful
import Effectful.Error.Static
import Effectful.GenRandom
import NSO.Image.DataCube
import NSO.Image.Headers.Parse
import NSO.Image.Headers.Types
import NSO.Image.Headers.WCS
import Telescope.Data.Parser (runPureParser)
import NSO.Image.L1Input
import NSO.Image.Primary as Primary
import NSO.Image.Profile as Profile
import NSO.Image.Quantity as Quantity
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Wavelength
import NSO.Image.Headers
import Skeletest
import Skeletest.Predicate qualified as P
import Telescope.Data.Axes hiding (Axis)
import Telescope.Data.WCS
import Telescope.Fits as Fits hiding (Axis)
import Telescope.Fits.Header (Header (..), HeaderRecord (..), KeywordRecord (..), parseKeyword)


spec :: Spec
spec = describe "Fits Generation" $ do
  specWavProfile
  specWCS
  specL1Frames
  specHeader
  specDateHeaders


specHeader :: Spec
specHeader = describe "Header Keywords" $ do
  describe "HDU Info" $ do
    it "should include headers" $ do
      let depth = DataHDUInfo :: Temperature
      let h = toHeader depth
      lookupKeyword "EXTNAME" h `shouldBe` Just (String "Temperature")
      lookupKeyword "BTYPE" h `shouldBe` Just (String "phys.temperature")
      lookupKeyword "BUNIT" h `shouldBe` Just (String "K")

  describe "Primary" $ do
    it "should include datacenter" $ do
      h <- toHeader <$> genPrimary
      lookupKeyword "LEVEL" h `shouldBe` Just (Integer 2)
      lookupKeyword "PROP_ID" h `shouldBe` Just (String "pid_2_95")

    it "should include DKIST Operations" $ do
      h <- toHeader <$> genPrimary
      lookupKeyword "DSHEALTH" h `shouldBe` Just (String "GOOD")

    it "should include AdaptiveOptics" $ do
      h <- toHeader <$> genPrimary
      lookupKeyword "AO_LOCK" h `shouldBe` Just (Logic T)

    it "should include Telescope" $ do
      h <- toHeader <$> genPrimary
      lookupKeyword "TAZIMUTH" h `shouldSatisfy` P.just (P.con (Float P.anything))
      lookupKeyword "OBSGEO_X" h `shouldSatisfy` P.just (P.con (Float P.anything))
      lookupKeyword "TELTRACK" h `shouldSatisfy` P.just (P.con (String P.anything))

    it "should include Observation" $ do
      h <- toHeader <$> genPrimary
      lookupKeyword "ORIGIN" h `shouldBe` Just (String "National Solar Observatory")
      lookupKeyword "INSTRUME" h `shouldBe` Just (String "VISP")

  describe "Quantities" $ do
    it "should include Common and Data HDU Headers" $ do
      h <- toHeader <$> genQuantity
      lookupKeyword "BTYPE" h `shouldBe` Just (String "phys.temperature")
      lookupKeyword "BZERO" h `shouldBe` Just (Integer 0)
      lookupKeyword "DATAMIN" h `shouldBe` Just (Float 1.0)

    it "should include pcs headers" $ do
      let hx = toHeader $ QuantityPCs @WCSMain @X (PC 1) (PC 2) (PC 3)
      axisN @(HDUAxis QuantityAxes X) `shouldBe` 2
      lookupKeyword "PC2_1" hx `shouldSatisfy` P.just P.anything
      lookupKeyword "PC2_2" hx `shouldSatisfy` P.just P.anything
      lookupKeyword "PC2_3" hx `shouldSatisfy` P.just P.anything

      let hya = toHeader $ QuantityPCs @A @Y (PC 1) (PC 2) (PC 3)
      axisN @(HDUAxis QuantityAxes Y) `shouldBe` 3
      lookupKeyword "PC3_1A" hya `shouldSatisfy` P.just P.anything
      lookupKeyword "PC3_2A" hya `shouldSatisfy` P.just P.anything
      lookupKeyword "PC3_3A" hya `shouldSatisfy` P.just P.anything

    it "should include WCS headers" $ do
      q <- genQuantity
      let h = toHeader q
      lookupKeyword "WCSNAME" h `shouldBe` Just (String "Helioprojective Cartesian")
      lookupKeyword "CRPIX1" h `shouldSatisfy` P.just P.anything
      lookupKeyword "PC1_1" h `shouldSatisfy` P.just P.anything
      lookupKeyword "PC1_3" h `shouldSatisfy` P.just P.anything
      lookupKeyword "PC3_1" h `shouldSatisfy` P.just P.anything
      lookupKeyword "PC1_1A" h `shouldSatisfy` P.just P.anything
      lookupKeyword "WCSNAMEA" h `shouldBe` Just (String "Equatorial equinox J2000")
      lookupKeyword "CUNIT1A" h `shouldSatisfy` P.just P.anything
      lookupKeyword "WCSVALID" h `shouldBe` Just (Logic T)

  describe "Porfiles" $ do
    it "should include Common and Data HDU Headers" $ do
      h <- toHeader <$> genProfile
      lookupKeyword "BTYPE" h `shouldBe` Just (String "spect.line.profile")
      lookupKeyword "BZERO" h `shouldBe` Just (Integer 0)
      lookupKeyword "DATAMIN" h `shouldBe` Just (Float 1.0)

    it "should include WCS headers" $ do
      h <- toHeader <$> genProfile
      lookupKeyword "WCSNAME" h `shouldBe` Just (String "Helioprojective Cartesian")
      lookupKeyword "CRPIX1" h `shouldSatisfy` P.just P.anything
      lookupKeyword "PC1_1" h `shouldSatisfy` P.just P.anything
      lookupKeyword "PC1_3" h `shouldSatisfy` P.just P.anything
      lookupKeyword "PC2_2" h `shouldSatisfy` P.just P.anything
      lookupKeyword "PC1_1A" h `shouldSatisfy` P.just P.anything
      lookupKeyword "WCSNAMEA" h `shouldBe` Just (String "Equatorial equinox J2000")
      lookupKeyword "CUNIT2A" h `shouldSatisfy` P.just P.anything
      lookupKeyword "WCSVALID" h `shouldBe` Just (Logic T)
 where
  genPrimary :: IO PrimaryHeader
  genPrimary = do
    L1HeaderFix l1 <- getFixture
    runGen $ Primary.primaryHeader (Id "inv.ID") l1

  genQuantity :: IO (QuantityHeader Temperature)
  genQuantity = do
    DataCommonFix common <- getFixture
    L1HeaderFix l1 <- getFixture
    wcs <- runGen $ Quantity.wcsHeader slice l1 :: IO (WCSHeader QuantityAxes)
    pure $ QuantityHeader @Temperature DataHDUInfo common wcs

  genProfile :: IO (ProfileHeader Orig630)
  genProfile = do
    DataCommonFix common <- getFixture
    L1HeaderFix l1 <- getFixture
    let wp = WavProfile 0 1 FeI
    wcs <- runGen $ Profile.wcsHeader wp slice l1
    pure $ ProfileHeader @Orig630 DataHDUInfo common wcs

  slice = SliceXY{pixelsPerBin = 7, pixelBeg = 100, pixelEnd = 800, frameBeg = 10, frameEnd = 20}

  runGen =
    runEff . runErrorNoCallStackWith @GenerateError throwM . runGenerateError . runGenRandom


specWCS :: Spec
specWCS = describe "WCS" $ do
  describe "golden 2_114 headers" $ do
    let h = keys_2_114
    it "should find yn xn" $ do
      (x, y) <- runErrorIO $ requireWCSAxes h
      y `shouldBe` Axis 3
      x `shouldBe` Axis 1

    it "requirePCs" $ do
      pcs <- runErrorIO $ do
        (x, y) <- requireWCSAxes h
        requirePCs @WCSMain x y h
      pcs.yy `shouldBe` PC 33
      pcs.yx `shouldBe` PC 31
      pcs.xx `shouldBe` PC 11
      pcs.xy `shouldBe` PC 13

    it "wcs keys" $ do
      (x, y) <- runErrorIO $ requireWCSAxes h
      ky <- runErrorIO $ wcsDummyY @WCSMain y slice h
      ky.ctype `shouldBe` Key ctypeY

      kx <- runErrorIO $ wcsSlitX @WCSMain x slice h
      kx.ctype `shouldBe` Key ctypeX

  describe "incorrect 1_118 headers" $ do
    let h = keys_1_118
    it "should find yn xn" $ do
      (x, y) <- runErrorIO $ requireWCSAxes h
      x `shouldBe` Axis 2
      y `shouldBe` Axis 3

    it "wcs keys y" $ do
      (_, y) <- runErrorIO $ requireWCSAxes h
      ky <- runErrorIO $ wcsDummyY @WCSMain y slice h
      ky.ctype `shouldBe` Key ctypeY

    it "wcs keys x" $ do
      (x, _) <- runErrorIO $ requireWCSAxes h
      kx <- runErrorIO $ wcsSlitX @WCSMain x slice h
      kx.ctype `shouldBe` Key ctypeX

  describe "binning adjustment" $ do
    it "should preserve CRVAL" $ do
      let w2 = adjustSlitX slice wcsX
      w2.crval `shouldBe` wcsX.crval

    it "should scale CDELT up by pixelsPerBin. fewer pixels = higher delta " $ do
      let w2 = adjustSlitX slice wcsX
      -- should just be the pixels per bin
      w2.cdelt.ktype `shouldSatisfy` P.gt wcsX.cdelt.ktype
      w2.cdelt.ktype `shouldSatisfy` P.approx P.tol (wcsX.cdelt.ktype * fromIntegral slice.pixelsPerBin)
      w2.cdelt.ktype `shouldBe` 7

    it "should scale CRPIX down by pixelsPerBin, and translate it down by begPixel" $ do
      let w2 = adjustSlitX slice wcsX
      -- 800 is exactly 700 higher than starting pixel, which should be 100 bins
      w2.crpix.ktype `shouldSatisfy` P.lt wcsX.crpix.ktype
      w2.crpix.ktype `shouldSatisfy` P.lt (wcsX.crpix.ktype / fromIntegral slice.pixelsPerBin)
      w2.crpix.ktype `shouldSatisfy` P.approx within1 100

  describe "dummy y frame adjustment" $ do
    it "should preserve CRVAL and CDELT" $ do
      let w2 = adjustDummyY slice wcsY
      w2.crval `shouldBe` wcsY.crval
      w2.cdelt `shouldBe` wcsY.cdelt

    it "should translate CRPIX" $ do
      let w2 = adjustDummyY slice wcsY
      w2.crpix.ktype `shouldBe` wcsY.crpix.ktype - fromIntegral slice.frameBeg
 where
  within1 = P.tol{P.abs = 1.0}

  -- sample spans 1400 pixels = 200 bins * 7 pixels, starting at 100
  -- sample assumes approx 500 frames, starting at 10 (and ending at 490, but that's not relevant to the calculation)
  slice = SliceXY{pixelsPerBin = 7, pixelBeg = 100, pixelEnd = 1500, frameBeg = 10, frameEnd = 490}

  -- sample is just 1 to 1600, with a CDELT of 1
  wcsX = WCSAxisKeywords{cunit = Key "unit", ctype = Key "type", cdelt = Key 1, crpix = Key 800, crval = Key 800}
  wcsY = WCSAxisKeywords{cunit = Key "unit", ctype = Key "type", cdelt = Key 1, crpix = Key 200, crval = Key 200}


specWavProfile :: Spec
specWavProfile = do
  describe "wavProfile" $ do
    describe "delta" $ do
      it "avgDelta should calc simple" $ do
        avgDelta simpleNums `shouldBe` 1000

      it "should calc simple delta" $ do
        (wavProfile FeI simple).delta `shouldBe` 0.1

      it "should calc regular delta" $ do
        round5 (wavProfile FeI wav630).delta `shouldBe` round5 (0.00128 * 10)

    describe "pixel" $ do
      it "should be exactly center in simple" $ do
        pixel0 1000 simpleNums `shouldBe` 3.5

      it "< positive index in wav630" $ do
        let p = wavProfile FeI wav630
        p.pixel `shouldSatisfy` P.lt 4

      it "> last negative index in wav630" $ do
        let px = (wavProfile FeI wav630).pixel
        px `shouldSatisfy` P.gt 3


simple :: DataCube '[Wavelength (Center 630 MA)]
simple = DataCube $ M.delay @Ix1 @P $ M.fromLists' Seq simpleNums


simpleNums :: [Float]
simpleNums = [-2500, -1500, -500, 500, 1500, 2500, 3500, 4500]


-- Actual raw data from profile. In original milliangstroms
wav630 :: DataCube '[Wavelength (Center 630 MA)]
wav630 =
  DataCube $
    M.delay @Ix1 @P $
      M.fromLists'
        Seq
        [-288.79998779, -160.80000305, -32.79999924, 95.19999695, 223.19999695, 351.20001221, 479.20001221, 607.20001221, 735.20001221, 863.20001221, 991.20001221, 1119.19995117, 1247.19995117, 1375.19995117, 1503.19995117]


runErrorIO :: Eff [Error ParseError, IOE] a -> IO a
runErrorIO eff = do
  res <- runEff $ runErrorNoCallStack eff
  case res of
    Left e -> throwM e
    Right a -> pure a


specDateHeaders :: Spec
specDateHeaders = do
  describe "DateTime" $ do
    it "should parse UTCTimes without Z" $ do
      let res :: Either ParseError DateTime = runPureParser $ parseKeywordValue (String "2023-05-01T18:53:59.504")
      res `shouldSatisfy` P.right P.anything


specL1Frames :: Spec
specL1Frames = do
  describe "l1 frame parse" $ do
    let path = Path "VISP_2023_05_01T19_14_27_229_00630200_I_AOPPO_L1.fits"
    it "should parse stokes" $ do
      Just f <- pure $ runParseFileName path
      f.stokes `shouldBe` I

    it "should parse wavelength" $ do
      Just f <- pure $ runParseFileName path
      f.wavelength `shouldBe` Wavelength 630.2

    it "should parse datetime" $ do
      Just f <- pure $ runParseFileName path
      show f.timestamp `shouldBe` "2023-05-01 19:14:27.229 UTC"

    it "should include original filename" $ do
      Just f <- pure $ runParseFileName path
      Path f.file.filePath `shouldBe` path


data WCSAxesFix = WCSAxesFix {x :: Axis X, y :: Axis Y}
instance Fixture WCSAxesFix where
  fixtureAction = runErrorIO $ do
    (x, y) <- requireWCSAxes keys_2_114
    pure $ noCleanup $ WCSAxesFix x y


data FrameCubeFix = FrameCubeFix {cube :: DataCube [SlitX, Depth]}
instance Fixture FrameCubeFix where
  fixtureAction = do
    let cube = DataCube $ M.delay $ M.fromLists' @P Seq [[1, 2, 3], [4, 5, 6]] :: DataCube [SlitX, Depth]
    pure $ noCleanup $ FrameCubeFix cube


data DataCommonFix = DataCommonFix {common :: DataCommon}
instance Fixture DataCommonFix where
  fixtureAction = do
    now <- getCurrentTime
    FrameCubeFix cube <- getFixture
    common <- dataCommon now cube
    pure $ noCleanup $ DataCommonFix common


data L1HeaderFix = L1HeaderFix {header :: Header}
instance Fixture L1HeaderFix where
  fixtureAction = do
    bs <- BS.readFile "./docs/l1.fits"
    fits <- Fits.decode bs
    case fits.extensions of
      [BinTable b] -> do
        pure $ noCleanup $ L1HeaderFix b.header
      _ -> fail "Expected [BinTable] Extensions"


keys_2_114 :: Header
keys_2_114 =
  -- Adjusted for each checking in some cases
  Header $
    fmap
      Keyword
      [ KeywordRecord "CRPIX1" (Float 1000) Nothing
      , KeywordRecord "CRPIX2" (Float 495.99999999999994) Nothing
      , KeywordRecord "CRPIX3" (Float 1303.4156199186928) Nothing
      , KeywordRecord "CRPIX1A" (Float 1244.200607073661) Nothing
      , KeywordRecord "CRPIX2A" (Float 496.0000000000001) Nothing
      , KeywordRecord "CRPIX3A" (Float 1303.4155132947976) Nothing
      , KeywordRecord "CRVAL1" (Float 100) Nothing
      , KeywordRecord "CRVAL2" (Float 589.60009) Nothing
      , KeywordRecord "CRVAL3" (Float (-32.89313312887808)) Nothing
      , KeywordRecord "CRVAL1A" (Float (-8.93437541160866)) Nothing
      , KeywordRecord "CRVAL2A" (Float 589.60009) Nothing
      , KeywordRecord "CRVAL3A" (Float 201.3284644221418) Nothing
      , KeywordRecord "CDELT1" (Float 0.1) Nothing
      , KeywordRecord "CDELT2" (Float 0.001052397146603632) Nothing
      , KeywordRecord "CDELT3" (Float 0.053406899773509015) Nothing
      , KeywordRecord "CDELT1A" (Float 6.62718784542916E-06) Nothing
      , KeywordRecord "CDELT2A" (Float 0.001052397146603632) Nothing
      , KeywordRecord "CDELT3A" (Float 1.48352499370858E-05) Nothing
      , KeywordRecord "CUNIT1" (String "arcsec  ") Nothing
      , KeywordRecord "CUNIT2" (String "nm      ") Nothing
      , KeywordRecord "CUNIT3" (String "arcsec  ") Nothing
      , KeywordRecord "CUNIT1A" (String "deg     ") Nothing
      , KeywordRecord "CUNIT2A" (String "nm      ") Nothing
      , KeywordRecord "CUNIT3A" (String "deg     ") Nothing
      , KeywordRecord "CUNIT3A" (String "deg     ") Nothing
      , KeywordRecord "CTYPE1" (String "HPLT-TAN") Nothing
      , KeywordRecord "CTYPE2" (String "AWAV    ") Nothing
      , KeywordRecord "CTYPE3" (String "HPLN-TAN") Nothing
      , KeywordRecord "CTYPE1A" (String "DEC--TAN") Nothing
      , KeywordRecord "CTYPE2A" (String "AWAV    ") Nothing
      , KeywordRecord "CTYPE3A" (String "RA---TAN") Nothing
      , KeywordRecord "PC1_1" (Float 11) Nothing
      , KeywordRecord "PC1_2" (Float 12) Nothing
      , KeywordRecord "PC1_3" (Float 13) Nothing
      , KeywordRecord "PC2_1" (Float 21) Nothing
      , KeywordRecord "PC2_2" (Float 22) Nothing
      , KeywordRecord "PC2_3" (Float 23) Nothing
      , KeywordRecord "PC3_1" (Float 31) Nothing
      , KeywordRecord "PC3_2" (Float 32) Nothing
      , KeywordRecord "PC3_3" (Float 33) Nothing
      , KeywordRecord "PC1_1A" (Float 0.8847943667398696) Nothing
      , KeywordRecord "PC1_2A" (Float 0.0) Nothing
      , KeywordRecord "PC1_3A" (Float 0.987368510792844) Nothing
      , KeywordRecord "PC2_1A" (Float 0.0) Nothing
      , KeywordRecord "PC2_2A" (Float 1.0) Nothing
      , KeywordRecord "PC2_3A" (Float 0.0) Nothing
      , KeywordRecord "PC3_1A" (Float 0.19682287694734163) Nothing
      , KeywordRecord "PC3_2A" (Float 0.0) Nothing
      , KeywordRecord "PC3_3A" (Float (-0.8860769143505891)) Nothing
      , KeywordRecord "LONPOLE" (Float 180.0) Nothing
      , KeywordRecord "LONPOLEA" (Float 180.0) Nothing
      , KeywordRecord "ZNAXIS1" (Integer 2000) Nothing
      ]


keys_1_118 :: Header
keys_1_118 =
  Header $
    fmap
      Keyword
      [ KeywordRecord "CRPIX1" (Float (-22.97932885096441)) Nothing
      , KeywordRecord "CRPIX2" (Float 477.0) Nothing
      , KeywordRecord "CRPIX3" (Float 17.52017812719757) Nothing
      , KeywordRecord "CRPIX1A" (Float (-22.97938348623828)) Nothing
      , KeywordRecord "CRPIX2A" (Float 477.0) Nothing
      , KeywordRecord "CRPIX3A" (Float 17.5164360061172) Nothing
      , KeywordRecord "CRVAL1" (Float 630.2051) Nothing
      , KeywordRecord "CRVAL2" (Float (-407.0004378228302)) Nothing
      , KeywordRecord "CRVAL3" (Float (-480.0037505031069)) Nothing
      , KeywordRecord "CRVAL1A" (Float 630.2051) Nothing
      , KeywordRecord "CRVAL2A" (Float 22.15633921882706) Nothing
      , KeywordRecord "CRVAL3A" (Float 70.70918101801153) Nothing
      , KeywordRecord "CDELT1" (Float 0.00162511509639976) Nothing
      , KeywordRecord "CDELT2" (Float 0.2134568481952311) Nothing
      , KeywordRecord "CDELT3" (Float 0.2134568481952311) Nothing
      , KeywordRecord "CDELT1A" (Float 0.00162511509639976) Nothing
      , KeywordRecord "CDELT2A" (Float 5.92935689431197E-05) Nothing
      , KeywordRecord "CDELT3A" (Float 5.92935689431197E-05) Nothing
      , KeywordRecord "CUNIT1" (String "nm      ") Nothing
      , KeywordRecord "CUNIT2" (String "arcsec  ") Nothing
      , KeywordRecord "CUNIT3" (String "arcsec  ") Nothing
      , KeywordRecord "CUNIT1A" (String "nm      ") Nothing
      , KeywordRecord "CUNIT2A" (String "deg     ") Nothing
      , KeywordRecord "CUNIT3A" (String "deg     ") Nothing
      , KeywordRecord "CTYPE1" (String "AWAV    ") Nothing
      , KeywordRecord "CTYPE2" (String "HPLT-TAN") Nothing
      , KeywordRecord "CTYPE3" (String "HPLN-TAN") Nothing
      , KeywordRecord "CTYPE1A" (String "AWAV    ") Nothing
      , KeywordRecord "CTYPE2A" (String "DEC--TAN") Nothing
      , KeywordRecord "CTYPE3A" (String "RA---TAN") Nothing
      , KeywordRecord "PC1_1" (Float 7.605732144392837) Nothing
      , KeywordRecord "PC1_2" (Float 0.0) Nothing
      , KeywordRecord "PC1_3" (Float (-0.00055119003270320)) Nothing
      , KeywordRecord "PC2_1" (Float 0.02996987746323728) Nothing
      , KeywordRecord "PC2_2" (Float 0.0) Nothing
      , KeywordRecord "PC2_3" (Float 0.1400743319547421) Nothing
      , KeywordRecord "PC3_1" (Float 0.0) Nothing
      , KeywordRecord "PC3_2" (Float 615.3410316693107) Nothing
      , KeywordRecord "PC3_3" (Float 0.0) Nothing
      , KeywordRecord "PC1_1A" (Float (-7.364415206124172)) Nothing
      , KeywordRecord "PC1_2A" (Float 0.0) Nothing
      , KeywordRecord "PC1_3A" (Float (-0.03521686283739144)) Nothing
      , KeywordRecord "PC2_1A" (Float (-1.907264089997582)) Nothing
      , KeywordRecord "PC2_2A" (Float 0.0) Nothing
      , KeywordRecord "PC2_3A" (Float 0.1355024888283506) Nothing
      , KeywordRecord "PC3_1A" (Float 0.0) Nothing
      , KeywordRecord "PC3_2A" (Float 615.3410316693107) Nothing
      , KeywordRecord "PC3_3A" (Float 0.0) Nothing
      , KeywordRecord "LONPOLE" (Float 180.0) Nothing
      , KeywordRecord "LONPOLEA" (Float 180.0) Nothing
      , KeywordRecord "ZNAXIS1" (Integer 2555) Nothing
      ]


ctypeY :: Text
ctypeY = "HPLN-TAN"


ctypeX :: Text
ctypeX = "HPLT-TAN"
