{-# LANGUAGE OverloadedLists #-}

module Test.BlancaSpec where

import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Comp (..), D, Ix2 (..), Ix3, IxN (..), Sz (..))
import Data.Massiv.Array qualified as M
import Effectful
import Effectful.Error.Static
import NSO.Image.Blanca
import NSO.Image.Types.Frame
import NSO.Image.Types.Profile
import NSO.Prelude
import NSO.Types.Wavelength
import Skeletest
import Telescope.Data.DataCube


spec :: Spec
spec = describe "NSO.Image.Blanca" $ do
  describe "wavBreaks" $ do
    it "identifies a line" $ do
      wavBreaks [LineId 23] `shouldBe` Right (Arms [ArmWavBreak FeI630 1])

    it "errors if line missing" $ do
      wavBreaks [LineId 1, LineId 23] `shouldBe` Left (LineId 1)

    it "identifies multiple lines" $ do
      print $ ArmWavBreak FeI630 20
      case wavBreaks (fmap LineId [23, 23, 9, 9, 9, 4]) of
        Left err -> failTest $ show err
        Right (Arms [a, b, c]) -> do
          a.line `shouldBe` FeI630
          a.length `shouldBe` 2

          b.line `shouldBe` NaID
          b.length `shouldBe` 3

          c.line `shouldBe` CaII854
          c.length `shouldBe` 1
        Right arms -> failTest $ "Wrong number of arms: " <> show arms

  describe "splitWavs" $ do
    it "breaks 1 arm" $ do
      let breaks = Arms [ArmWavBreak FeI630 3]
      let offs = fmap WavOffset [1, 2, 3]
      res <- runEff $ runErrorNoCallStack @BlancaError $ splitWavs breaks offs
      case res of
        Left err -> failTest $ show err
        Right a -> a `shouldBe` Arms [NE.fromList offs]

      let long = fmap WavOffset [1, 2, 3, 4]
      res2 <- runEff $ runErrorNoCallStack @BlancaError $ splitWavs breaks long
      case res2 of
        Left err -> failTest $ show err
        Right a -> a `shouldBe` Arms [NE.fromList offs]

    it "breaks 2 arms" $ do
      let breaks = Arms [ArmWavBreak FeI630 3, ArmWavBreak NaID 2]
      let offs = fmap WavOffset [1, 2, 3, 4, 5]
      res <- runEff $ runErrorNoCallStack @BlancaError $ splitWavs breaks offs
      case res of
        Left err -> failTest $ show err
        Right a -> a `shouldBe` Arms [[WavOffset 1, WavOffset 2, WavOffset 3], [WavOffset 4, WavOffset 5]]

  describe "splitFrameIntoArms" $ do
    it "splits" $ do
      -- only 1 wav belongs to FeI630
      -- two wavs belong to NaID
      let metas = Arms [ArmWavMeta FeI630 1 0 (WavOffset 1), ArmWavMeta NaID 2 0 (WavOffset 1)]
      res <- runEff $ runErrorNoCallStack @BlancaError $ splitFrameIntoArms metas frameCombined
      case res of
        Left err -> failTest $ "Split Frame: " <> show err
        Right (Arms [armFeI, armNaD]) -> do
          M.toLists armFeI.array `shouldBe` [[[0]]]
          M.toLists armNaD.array `shouldBe` [[[1], [2]]]
        Right arms -> failTest $ "Wrong Arms: " <> show arms

  describe "collate" $ do
    it "armsFrames" $ do
      let farms = Frames $ NE.fromList $ replicate 10 $ Arms [ArmWavMeta FeI630 1 0 (WavOffset 1), ArmWavMeta NaID 1 0 (WavOffset 1)]
      let metasByArm = armsFrames farms
      length metasByArm.arms `shouldBe` 2
      Arms [framesFeI, _] <- pure metasByArm
      length framesFeI `shouldBe` 10

    it "frameArms" $ do
      let arms = Arms [Frames [1, 2, 3], Frames [10, 20, 30]] :: Arms (Frames Int)
      length (framesArms arms) `shouldBe` 3

    it "collates arm-frames into frame-arms" $ do
      let metas = Arms [ArmWavMeta FeI630 1 0 (WavOffset 1)]
          fitFrames = Arms [Frames $ NE.fromList $ replicate 10 $ ProfileImage undefined]
          origFrames = Arms [Frames $ NE.fromList $ replicate 10 $ ProfileImage undefined]
          frames = collateFramesArms metas fitFrames origFrames
      length frames `shouldBe` 10
      Frames (frame1 :| _) <- pure frames
      length frame1.arms `shouldBe` 1


-- 1 pixels, 3 wavs, 1 stokes
frameCombined :: DataCube [SlitX, CombinedArms (Wavelength MA), Stokes] Float
frameCombined = DataCube $ M.makeArray @D @Ix3 Seq (Sz (1 :> 3 :. 1)) pixel
 where
  pixel (slitX :> wav :. stokes) =
    fromIntegral $ slitX * 0 + stokes * 0 + wav
