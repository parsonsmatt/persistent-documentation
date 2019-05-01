module Data.SemiMap where

import           Data.Map (Map)
import qualified Data.Map as Map

-- | A newtype around 'Map' which uses @'Map.unionWith' '(<>)'@ for the
-- semigroup and monoid instance.
--
-- @since 0.1.0.0
newtype SemiMap k v = SemiMap { unSemiMap :: Map k v }
  deriving (Eq, Ord, Show)

instance (Ord k, Semigroup v) => Semigroup (SemiMap k v) where
  SemiMap m0 <> SemiMap m1 = SemiMap (Map.unionWith (<>) m0 m1)

instance (Ord k, Semigroup v) => Monoid (SemiMap k v) where
  mempty = SemiMap mempty
