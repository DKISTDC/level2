{-# LANGUAGE DefaultSignatures #-}

module Flow.Node where

import Data.Proxy
import GHC.Generics

class Node a where
    nodeName :: Proxy a -> String
    default nodeName :: (Generic a, GNode (Rep a)) => Proxy a -> String
    nodeName _ = gnodeName (from (undefined :: a))

class Input a where
    nodeNames :: Proxy a -> [String]
    default nodeNames :: Node a => Proxy a -> [String]
    nodeNames p = [nodeName p]

instance Input () where
    nodeNames _ = []

instance (Node a, Node b) => Input (a, b) where
    nodeNames _ = [nodeName @a Proxy, nodeName @b Proxy]

instance (Node a, Node b, Node c) => Input (a, b, c) where
    nodeNames _ = [nodeName @a Proxy, nodeName @b Proxy, nodeName @c Proxy]

-- | Generic NodeName
class GNode f where
    gnodeName :: f p -> String

instance (Datatype d) => GNode (D1 d f) where
    gnodeName = datatypeName
