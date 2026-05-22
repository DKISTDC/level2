module Test.FilePathSpec where

import Effectful
import Effectful.FileSystem
import Effectful.Log
import NSO.Files.Image
import NSO.Files.RemoteFolder
import NSO.Files.Scratch
import NSO.Prelude
import NSO.Types.Common
import Skeletest


spec :: Spec
spec = do
  describe "Scratch" $ do
    it "correct mounted path" $ do
      let r = Remote "fake-collection" "Data/level2/inbox"
      let inbox = Config "/Users/seanhess" r
      mp <- runTestScratch @Inbox inbox $ mountedPath @Inbox "./files/something.txt"
      mp.filePath `shouldBe` "/Users/seanhess/Data/level2/inbox/files/something.txt"

  describe "Output" $ do
    it "output folder path with underscores" $ do
      let p = inversionDir (Id "pid_2_114") (Id "inv.ASDFG")
      p `shouldBe` "pid_2_114/inv_ASDFG"

    it "output folder mounted path " $ do
      let r = Remote "fake-collection" "Data/level2/output"
      let output = Config "/Users/seanhess" r
      let p = inversionDir (Id "pid_2_114") (Id "inv.ASDFG")
      mp <- runTestScratch @Output output $ mountedPath @Output p
      mp.filePath `shouldBe` "/Users/seanhess/Data/level2/output/pid_2_114/inv_ASDFG"


runTestScratch :: forall s a. Config s -> Eff [Scratch s, FileSystem, Log, IOE] a -> IO a
runTestScratch cfg = runEff . runLoggerIgnore . runFileSystem . runScratch cfg
