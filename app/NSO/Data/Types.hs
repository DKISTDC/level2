{-# LANGUAGE DerivingVia #-}

module NSO.Data.Types where

import Data.Aeson (FromJSON (..), withText)
import Data.List qualified as L
import NSO.Prelude
import Rel8
import Web.Hyperbole (Route)

newtype Id a = Id {fromId :: Text}
  deriving newtype (Show, Eq, Ord, DBType, FromJSON, Route)
  deriving (Generic)

data Stokes = I | Q | U | V
  deriving (Show, Read, Eq)
  deriving (DBType) via ReadShow Stokes

data ObserveFrames

newtype StokesParameters = StokesParameters [Stokes]
  deriving newtype (DBType, Eq, Monoid)

instance Show StokesParameters where
  show (StokesParameters ss) = mconcat $ fmap show ss

instance FromJSON StokesParameters where
  parseJSON = withText "Stokes Params" $ \t -> do
    sps <- mapM parseChar $ cs t
    pure $ StokesParameters sps
   where
    parseChar 'I' = pure I
    parseChar 'Q' = pure Q
    parseChar 'U' = pure U
    parseChar 'V' = pure V
    parseChar c = fail $ "Expected Stokes param (IQUV) but got: " <> [c]

instance Semigroup StokesParameters where
  (StokesParameters a) <> (StokesParameters b) = StokesParameters . L.nub $ a <> b
