
module Tsds.Joins where

import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)
import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set

import Tsds.Types

join2 :: Applicative f => f a -> f b -> f (a, b)
join2 = liftA2 (,)

leftJoin :: Alternative f => f a -> f a1 -> f (a, Maybe a1)
leftJoin t1 t2 = join2 t1 (optional t2)
