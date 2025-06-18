{-# LANGUAGE OverloadedLists #-}

module Test.BlancaSpec where

import Data.Massiv.Array (Comp (..), D, Ix2 (..), Ix3, IxN (..), Sz (..))
import Data.Massiv.Array qualified as M
import Effectful
import Effectful.Error.Static
import NSO.Image.Blanca
import NSO.Image.Headers.Types
import NSO.Image.Types.Profile
import NSO.Prelude
import NSO.Types.Wavelength
import Skeletest
import Telescope.Data.DataCube


spec :: Spec
spec = describe "NSO.Image.Blanca" $ do
  describe "wavBreaks" $ withMarkers ["focus"] $ do
    it "identifies a line" $ do
      wavBreaks [LineId 23] `shouldBe` Right (Arms [ArmWavBreak FeI 1])

    it "errors if line missing" $ do
      wavBreaks [LineId 1, LineId 23] `shouldBe` Left (LineId 1)

    it "identifies multiple lines" $ do
      print $ ArmWavBreak FeI 20
      case wavBreaks (fmap LineId [23, 23, 9, 9, 9, 4]) of
        Left err -> failTest $ show err
        Right (Arms [a, b, c]) -> do
          a.line `shouldBe` FeI
          a.length `shouldBe` 2

          b.line `shouldBe` NaD
          b.length `shouldBe` 3

          c.line `shouldBe` CaII CaII_854
          c.length `shouldBe` 1
        Right arms -> failTest $ "Wrong number of arms: " <> show arms

  describe "splitWavs" $ withMarkers ["focus"] $ do
    it "breaks 1 arm" $ do
      let breaks = Arms [ArmWavBreak FeI 3]
      let offs = fmap WavOffset [1, 2, 3]
      splitWavs breaks offs `shouldBe` Arms [offs]

      let long = fmap WavOffset [1, 2, 3, 4]
      splitWavs breaks long `shouldBe` Arms [offs]

    it "breaks 2 arms" $ do
      let breaks = Arms [ArmWavBreak FeI 3, ArmWavBreak NaD 2]
      let offs = fmap WavOffset [1, 2, 3, 4, 5]
      splitWavs breaks offs `shouldBe` Arms [[WavOffset 1, WavOffset 2, WavOffset 3], [WavOffset 4, WavOffset 5]]

  describe "splitFrameIntoArms" $ withMarkers ["focus"] $ do
    it "splits" $ do
      -- only 1 wav belongs to FeI
      -- two wavs belong to NaD
      let metas = Arms [ArmWavMeta FeI 1 0 (WavOffset 1), ArmWavMeta NaD 2 0 (WavOffset 1)]
      res <- runEff $ runErrorNoCallStack @BlancaError $ splitFrameIntoArms metas frameCombined
      case res of
        Left err -> failTest $ "Split Frame: " <> show err
        Right (Arms [armFeI, armNaD]) -> do
          M.toLists armFeI.array `shouldBe` [[[0]]]
          M.toLists armNaD.array `shouldBe` [[[1], [2]]]
        Right arms -> failTest $ "Wrong Arms: " <> show arms


-- 1 pixels, 3 wavs, 1 stokes
frameCombined :: DataCube [SlitX, CombinedArms (Wavelength MA), Stokes] Float
frameCombined = DataCube $ M.makeArray @D @Ix3 Seq (Sz (1 :> 3 :. 1)) pixel
 where
  pixel (slitX :> wav :. stokes) =
    fromIntegral $ slitX * 0 + stokes * 0 + wav
