module Data.MonoidMap
    (
      MonoidMap(..)
    , singleton
    , lookup
    , lookup'
    ) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Set (Set)

import Prelude hiding (lookup)

-- | A mapping that utilizes the semigroup instance of 'Set.Set' when
--   using '<>'.
newtype MonoidMap k v = MonoidMap
    { unMonoidMap :: Map k (Set v) }
    deriving (Show, Eq, Ord)

instance (Ord k, Ord v) => Semigroup (MonoidMap k v) where
    MonoidMap m1 <> MonoidMap m2
        = MonoidMap $ M.unionWith (<>) m1 m2

instance (Ord k, Ord v) => Monoid (MonoidMap k v) where
    mempty = MonoidMap M.empty

-- | Create a simple 'MonoidMap' from a single key and value
singleton :: Ord k => k -> v -> MonoidMap k v
singleton k = MonoidMap . M.singleton k . S.singleton

-- | Perform a lookup using key @k@. Returns a 'Set' wrapped in 'Maybe'.
lookup :: Ord k => k -> MonoidMap k v -> Maybe (Set v)
lookup k = M.lookup k . unMonoidMap

-- | Like 'lookup', but returns an empty 'Set' in the event of a failed lookup.
lookup' :: Ord k => k -> MonoidMap k v -> Set v
lookup' k = fromMaybe S.empty . lookup k
