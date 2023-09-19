module Effectful.GraphQL where

import Data.Morpheus.Client
import Effectful
import Effectful.Dispatch.Dynamic

data GraphQL :: Effect where
  -- Request :: Args a -> GraphQL m (ResponseStream a)
  Fetch :: Fetch a => Args a -> GraphQL m (Either (FetchError a) a)


