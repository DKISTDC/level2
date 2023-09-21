module NSO.Steps where

-- import Control.Lens
-- import Data.Proxy
-- import Data.Vinyl
-- import Data.Vinyl.Functor (Identity (..))
--

-- import Data.Diverse
-- import Data.Tagged
import Data.Text
import GHC.Records
import NSO.Prelude

newtype Id a = Id Text
  deriving (Show, Eq)

data PreprocessedDataset = PreprocessedDataset deriving (Show)
data Magnetogram = Magnetogram deriving (Show)
data Dopplergram = Dopplergram deriving (Show)
data PublishedDataset = PublishedDataset deriving (Show)

{--

testExtensible :: IO ()
testExtensible = do
  -- let id' = Id "dataset"
  --     -- dis = Discover id
  --     -- qua = Qualify id
  --     pre = Preprocessed'{id = id', preprocessed = PreprocessedDataset}
  --     dat = Preprocessed pre
  -- print $ dat.id
  print inv
  let dat = Inverted inv
  print $ grab @(Id Dataset) inv
  print $ dat.id
  print $ grab @Magnetogram inv
  let pub = Published $ PublishedDataset ./ inv
  print pub.id
  print $ grabL @"dopplergram" inv
  -- print dat.published
  pure ()

type Identified = '[Id Dataset]
type Qualified = Identified
type Preprocessed = PreprocessedDataset ': Identified
type Inverted = Tagged "dopplergram" Dopplergram ': Magnetogram ': Preprocessed
type Published = PublishedDataset ': Inverted

data Dataset a where
  Identified :: Many Identified -> Dataset Identified
  Qualified :: Many Identified -> Dataset Qualified
  Preprocessed :: Many Preprocessed -> Dataset Preprocessed
  Inverted :: Many Inverted -> Dataset Inverted
  Published :: Many Published -> Dataset Published

inv :: Many Inverted
inv = Tagged Dopplergram ./ Magnetogram ./ PreprocessedDataset ./ Id "woot" ./ nil

-- TODO: this isn't ideal, because you may end up with just the `Many f` being passed around
-- probably have to commit to using `grab @Preprocessed` instead of records

instance HasField "id" (Dataset a) (Id Dataset) where
  getField (Identified m) = grab m
  getField (Qualified m) = grab m
  getField (Preprocessed m) = grab m
  getField (Inverted m) = grab m
  getField (Published m) = grab m

-- instance HasField "preprocessed" (Dataset Preprocessed) PreprocessedDataset where
--   getField (Preprocessed m) = grab m
--
-- instance HasField "preprocessed" (Dataset Inverted) PreprocessedDataset where
--   getField (Inverted m) = grab m

-- instance HasField "dopplergram" (Dataset Inverted) Dopplergram where
--   getField (Inverted m) = grab m
--
-- instance HasField "dopplergram" (Dataset Published) Dopplergram where
--   getField (Published m) = grab m
--
-- instance HasField "published" (Dataset Published) PublishedDataset where
--   getField (Published m) = grab m
--
-- instance HasField "magnetogram" (Dataset Published) Magnetogram where
--   getField (Published m) = grab m
--
-- instance HasField "preprocessed" (Dataset Published) PreprocessedDataset where
--   getField (Published m) = grab m
--}

-- NOTE: The easiest way to do this is just make a bunch of records. Good autocomplete. Updates. Easy
-- we can assemble them separately too? Sure, you can just have a record with the extra information

newtype Identified' = Identified'
  { id :: Id Dataset
  }

newtype Qualified' = Qualified'
  { id :: Id Dataset
  }

data Preprocessed' = Preprocessed'
  { id :: Id Dataset
  , preprocessed :: PreprocessedDataset
  }

data Inverted' = Inverted'
  { id :: Id Dataset
  , preprocessed :: PreprocessedDataset
  , magnetogram :: Magnetogram
  , dopplergram :: Dopplergram
  }

data Published' = Published'
  { id :: Id Dataset
  , preprocessed :: PreprocessedDataset
  , magnetogram :: Magnetogram
  , dopplergram :: Dopplergram
  , published :: PublishedDataset
  }

-- NOTE: we don't need a GADT because we can just pass around the above objects
-- and write functions that depend on the data being available
data Dataset
  = Identified Identified'
  | Qualified Qualified'
  | Preprocessed Preprocessed'
  | Inverted Inverted'
  | Published Published'

instance HasField "id" Dataset (Id Dataset) where
  getField (Identified a) = a.id
  getField (Qualified a) = a.id
  getField (Preprocessed a) = a.id
  getField (Inverted a) = a.id
  getField (Published a) = a.id

{--
data Steps = Discovered | Qualified | Preprocessed | Inverted | Published
data Step a where
  Discover :: Id Dataset -> Step 'Discovered
  Qualify :: Id Dataset -> Step 'Qualified
  Preprocess :: (Id Dataset, PreprocessedDataset) -> Step 'Preprocessed
  Invert :: (Id Dataset, PreprocessedDataset, Magnetogram, Dopplergram) -> Step 'Inverted
  Publish :: (Id Dataset, PreprocessedDataset, Magnetogram, Dopplergram, PublishedDataset) -> Step 'Inverted -> Step 'Published

instance HasField "datasetId" (Step 'Discovered) (Id Dataset) where
  getField (Discover i) = i

instance HasField "datasetId" (Step 'Qualified) (Id Dataset) where
  getField (Qualify i) = i

instance HasField "datasetId" (Step 'Preprocessed) (Id Dataset) where
  getField (Preprocess i _) = i
--}

-- Ok, I'll TRY with some extensible records
-- vinyl: 34k, 75 in last 30
-- data-diverse: 18k, but 77 in last 30
-- extensible: 44 in last 30
