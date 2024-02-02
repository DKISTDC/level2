module Flow.Example where

import Effectful
import Effectful.Reader.Dynamic
import Flow
import GHC.Generics (Generic)
import Prelude


data A = A deriving (Show, Generic, Node, Input)
data B = B deriving (Show, Generic, Node, Input)
data C = C deriving (Show, Generic, Node, Input)
data D = D deriving (Show, Generic, Node, Input)
data Dataset = Dataset String deriving (Show, Generic, Node, Input)
data Final = Final A D Dataset deriving (Show, Generic, Node, Input)


-- easy to make an alias for my flow
type MyFlow es = (Reader Dataset :> es, Flow :> es)


dataset :: (MyFlow es) => Eff es Dataset
dataset = ask


-- TODO: I want workflow to look like this! Really easy to see the network
workflow :: (MyFlow es) => Eff es Final
workflow = do
  a <- stepA
  b <- stepB a
  c <- stepC a
  d <- stepD b c
  stepEnd a d


stepA :: (Flow :> es) => Eff es A
stepA = taskIO (const execA) ()
 where
  execA :: IO A
  execA = pure A


stepB :: (Flow :> es) => A -> Eff es B
stepB = taskIO execB
 where
  execB :: A -> IO B
  execB _ = pure B


stepC :: (Flow :> es) => A -> Eff es C
stepC = taskIO execC
 where
  execC :: A -> IO C
  execC _ = pure C


stepD :: (Flow :> es) => B -> C -> Eff es D
stepD b c = taskIO (uncurry execD) (b, c)
 where
  execD :: B -> C -> IO D
  execD _ _ = pure D


stepEnd :: (Reader Dataset :> es, Flow :> es) => A -> D -> Eff es Final
stepEnd a' d' = do
  ds <- dataset
  taskIO (\(a, d) -> execEnd a d ds) (a', d')
 where
  execEnd :: A -> D -> Dataset -> IO Final
  execEnd a d ds = do
    pure $ Final a d ds


test :: IO ()
test = do
  f <- runEff $ runReader (Dataset "hello") $ runFlowIO workflow
  putStrLn "RUN FLOW IO!"
  print f

  d <- runEff $ runReader (Dataset "net") $ runFlowDAG workflow
  putStrLn "\nRUN FLOW DAG"
  print d
