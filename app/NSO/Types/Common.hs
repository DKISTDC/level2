module NSO.Types.Common where

import Data.Aeson
import NSO.Prelude
import Rel8 (DBEq, DBType)
import Web.Hyperbole (Param (..), Route)

newtype Id a = Id {fromId :: Text}
  deriving newtype (Show, Read, Eq, Ord, DBType, FromJSON, Route, DBEq)
  deriving (Generic)

instance Param (Id a) where
  toParam (Id t) = t
  parseParam t = pure $ Id t

