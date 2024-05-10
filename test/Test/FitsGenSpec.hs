module Test.FitsGenSpec where

import Control.Monad.Catch (throwM)
import Data.Fits
import Data.Massiv.Array as M (Comp (..), Ix1, P, delay, fromLists')
import Effectful
import Effectful.Error.Static
import NSO.Fits.Generate.DimArray
import NSO.Fits.Generate.Headers.LiftL1
import NSO.Fits.Generate.Headers.Types
import NSO.Fits.Generate.Headers.WCS
import NSO.Fits.Generate.Profile
import NSO.Prelude
import NSO.Types.Wavelength
import Test.Syd


spec :: Spec
spec = describe "Fits Generation" $ do
  specWavProfile
  specWCS


ctypeY :: Text
ctypeY = "HPLN-TAN"


ctypeX :: Text
ctypeX = "HPLT-TAN"


specWCS :: Spec
specWCS = describe "WCS" $ do
  describe "golden 2_114 headers" $ do
    it "should find yn" $ do
      yn <- runErrorIO $ findCtypeAxis ctypeY keys_2_114
      yn `shouldBe` 3

    it "should find xn" $ do
      xn <- runErrorIO $ findCtypeAxis ctypeX keys_2_114
      xn `shouldBe` 1

    it "wcsDummyYKeys" $ do
      k <- runErrorIO $ wcsDummyYKeys @WCSMain keys_2_114
      k.ctype `shouldBe` Key ctypeY

    it "wcsDummyXKeys" $ do
      k <- runErrorIO $ wcsSlitXKeys @WCSMain 237 keys_2_114
      k.ctype `shouldBe` Key ctypeX

  describe "incorrect 1_118 headers" $ do
    it "should find yn" $ do
      yn <- runErrorIO $ findCtypeAxis ctypeY keys_1_118
      yn `shouldBe` 3

    it "should find xn" $ do
      xn <- runErrorIO $ findCtypeAxis ctypeX keys_1_118
      xn `shouldBe` 1

    it "wcsDummyYKeys" $ do
      k <- runErrorIO $ wcsDummyYKeys @WCSMain keys_1_118
      k.ctype `shouldBe` Key ctypeY

    it "wcsDummyXKeys" $ do
      k <- runErrorIO $ wcsSlitXKeys @WCSMain 237 keys_1_118
      k.ctype `shouldBe` Key ctypeX
 where
  keys_2_114 =
    Header $
      fmap
        Keyword
        [ KeywordRecord "CRPIX1" (Float 1244.1995308776818) Nothing
        , KeywordRecord "CRPIX2" (Float 495.99999999999994) Nothing
        , KeywordRecord "CRPIX3" (Float 1303.4156199186928) Nothing
        , KeywordRecord "CRPIX1A" (Float 1244.200607073661) Nothing
        , KeywordRecord "CRPIX2A" (Float 496.0000000000001) Nothing
        , KeywordRecord "CRPIX3A" (Float 1303.4155132947976) Nothing
        , KeywordRecord "CRVAL1" (Float 91.6717798950973) Nothing
        , KeywordRecord "CRVAL2" (Float 589.60009) Nothing
        , KeywordRecord "CRVAL3" (Float (-32.89313312887808)) Nothing
        , KeywordRecord "CRVAL1A" (Float (-8.93437541160866)) Nothing
        , KeywordRecord "CRVAL2A" (Float 589.60009) Nothing
        , KeywordRecord "CRVAL3A" (Float 201.3284644221418) Nothing
        , KeywordRecord "CDELT1" (Float 0.02385787624354498) Nothing
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
        , KeywordRecord "PC1_1" (Float 0.989725216270561) Nothing
        , KeywordRecord "PC1_2" (Float 0.0) Nothing
        , KeywordRecord "PC1_3" (Float 0.011515025422160527) Nothing
        , KeywordRecord "PC2_1" (Float 0.0) Nothing
        , KeywordRecord "PC2_2" (Float 1.0) Nothing
        , KeywordRecord "PC2_3" (Float 0.0) Nothing
        , KeywordRecord "PC3_1" (Float (-0.00232574119521603)) Nothing
        , KeywordRecord "PC3_2" (Float 0.0) Nothing
        , KeywordRecord "PC3_3" (Float 0.9896134027832052) Nothing
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
        , KeywordRecord "ZNAXIS1" (Integer 2545) Nothing
        ]

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


specWavProfile :: Spec
specWavProfile = do
  describe "wavProfile" $ do
    describe "delta" $ do
      it "avgDelta should calc simple" $ do
        avgDelta simpleNums `shouldBe` 0.1

      it "should calc simple delta" $ do
        (wavProfile simple).delta `shouldBe` 0.1

      it "should calc regular delta" $ do
        (wavProfile wav630).delta `shouldBe` (0.00128 * 10)

    describe "pixel" $ do
      it "should be exactly center in simple" $ do
        pixel0 0.1 simpleNums `shouldBe` 3.5

      it "< positive index in wav630" $ do
        let px = (wavProfile wav630).pixel
        px `shouldSatisfy` (< 4)

      it "> last negative index in wav630" $ do
        let px = (wavProfile wav630).pixel
        px `shouldSatisfy` (> 3)


simple :: DimArray '[Wavelength 630]
simple = DimArray $ M.delay @Ix1 @P $ M.fromLists' Seq simpleNums


simpleNums :: [Float]
simpleNums = [-0.25, -0.15, -0.05, 0.05, 0.15, 0.25, 0.35, 0.45]


-- Actual data. Every 10th element (so the spacing is larger than normal)
wav630 :: DimArray '[Wavelength 630]
wav630 =
  DimArray $
    M.delay @Ix1 @P $
      M.fromLists'
        Seq
        [-0.02888, -0.01608, -0.00328, 0.00952, 0.02232, 0.03512, 0.04792, 0.06072, 0.07352, 0.08632, 0.09912, 0.11191999, 0.12471999, 0.13752, 0.15031999]


runErrorIO :: Eff [Error LiftL1Error, IOE] a -> IO a
runErrorIO eff = do
  res <- runEff $ runErrorNoCallStack eff
  case res of
    Left e -> throwM e
    Right a -> pure a
