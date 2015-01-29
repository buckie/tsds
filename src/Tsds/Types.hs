
module Tsds.Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)
import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set

-- | A map encoded as a lookup function
data Table k v = Table
    { keys   :: Keys k
    , lookup :: k -> Maybe v
    }

instance Functor (Table k) where
    fmap f (Table ks g) = Table ks (fmap (fmap f) g)

instance Ord k => Applicative (Table k) where
    pure v =
        Table 1 (pure (pure v))

    Table s1 f <*> Table s2 x =
        Table (s1 * s2) (liftA2 (<*>) f x)

instance (Show k, Show v) => Show (Table k v) where
    show (Table ks f) = case ks of
        All    -> "<function>"
        Some s -> unlines (do
            k <- Set.toList s
            let Just v = f k
            return (show (k, v)) )

instance Ord k => Alternative (Table k) where
    empty =
        Table 0 (pure empty)

    Table ks1 f1 <|> Table ks2 f2 =
        Table (ks1 + ks2) (liftA2 (<|>) f1 f2)

-- | Encode a traditional map as a lookup function
from :: Ord k => Map k v -> Table k v
from m = Table
    { keys   = Some (Set.fromList (Map.keys m))
    , lookup = \k -> Map.lookup k m
    }

data Keys k = All | Some (Set k)

instance Ord k => Num (Keys k) where
    fromInteger 0         = Some Set.empty
    fromInteger n | n > 0 = All

    All     + _       = All
    _       + All     = All
    Some s1 + Some s2 = Some (Set.union s1 s2)

    All     * ks      = ks
    ks      * All     = ks
    Some s1 * Some s2 = Some (Set.intersection s1 s2)
