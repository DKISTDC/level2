{-# LANGUAGE LambdaCase #-}

module Flow
    ( Flow (..)
    , runFlowIO
    , DAG
    , runFlowDAG
    , Task (..)
    , taskIO
    , Node (..)
    , Input (..)
    ) where

import Data.Proxy
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Writer.Dynamic
import Flow.Node

data Flow :: Effect where
    RunTask :: (Node a, Input inp) => Task inp a -> inp -> Flow m a

newtype Task inp a = Task {runTask :: inp -> IO a}

type instance DispatchOf Flow = 'Dynamic

taskIO :: (Node a, Input inp, (Flow :> es)) => (inp -> IO a) -> inp -> Eff es a
taskIO ft i = send $ RunTask (Task ft) i

runFlowIO
    :: (IOE :> es)
    => Eff (Flow : es) a
    -> Eff es a
runFlowIO = interpret $ \_ -> \case
    (RunTask (Task ft) i) -> liftIO $ ft i

runFlowDAG
    :: forall es a
     . (IOE :> es)
    => Eff (Flow : es) a
    -> Eff es DAG
runFlowDAG = reinterpret (execWriterLocal @DAG) $ \_ -> \case
    (RunTask t _) -> runTaskDAG t

runTaskDAG
    :: forall a inp es
     . (Node a, Input inp, Writer DAG :> es)
    => Task inp a
    -> Eff es a
runTaskDAG _ = do
    let an = nodeName @a Proxy
    let ins = nodeNames @inp Proxy
    tell $ [(i, an) | i <- ins]

    -- WARNING: use error instead of requiring Flow to return a functor
    -- if necessary we could return defaults instead, but they are
    -- never used
    pure $ error "Network should not evaluate values"

type DAG = [(String, String)]
