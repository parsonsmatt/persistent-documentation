module Data.StrMap where

import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Text (Text)
import qualified Data.Text as Text

-- | A 'StrMap' is sort of like a 'HashMap', but sorts the keys on a 'Text'
-- representation. Additionally, it has more useful 'Semigroup' and 'Monoid'
-- instances that '(<>)' the values when present.
--
-- @since 0.1.0.0
newtype StrMap k a = StrMap (Map (AsStr k) a)
  deriving (Eq, Ord, Show)

instance Semigroup a => Semigroup (StrMap k a) where
  StrMap as <> StrMap bs = StrMap $
    Map.unionWith (<>) as bs

instance Semigroup a => Monoid (StrMap k a) where
  mempty = StrMap mempty

-- | Insert a value into a 'StrMap'.
--
-- @since 0.1.0.0
insert :: Show k => k -> a -> StrMap k a -> StrMap k a
insert k a (StrMap m) = StrMap (Map.insert (asStr k) a m)

-- | Lookup a value in the 'StrMap'.
--
-- @since 0.1.0.0
lookup :: Show k => k -> StrMap k a -> Maybe a
lookup k (StrMap m) = Map.lookup (asStr k) m

-- | A datatype for representing the keys of entries in a 'StrMap'.
-- Contains the original value as well as the 'Text'ual representation of
-- that value.
--
-- The 'Eq' and 'Ord' instances only use the 'Text' value.
--
-- @since 0.1.0.0
data AsStr k = AsStr { asStrText :: Text, asStrValue :: k } deriving Show

instance Eq (AsStr k) where
  AsStr a _ == AsStr b _ = a == b

instance Ord (AsStr k) where
  AsStr a _ `compare` AsStr b _ = compare a b

-- | Pack a value into an 'AsStr' of that value.
--
-- @since 0.1.0.0
asStr :: Show k => k -> AsStr k
asStr k = AsStr (Text.pack (show k)) k

