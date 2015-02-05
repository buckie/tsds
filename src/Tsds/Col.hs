
module Tsds.Col where

import BasePrelude

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M

import Data.Int ( Int8, Int16, Int32, Int64 )

newtype I64 = I64 Int64 deriving (Bounded)

class Eq a => Nullable a where
  nullFor :: a
  isNull :: a -> Bool
  is :: a -> a -> Bool

instance Nullable I64 where
  nullFor = minBound
  isNull (I64 x) = x == minBound
  is x y = case isNull x && isNull y of
                   True -> True
                   False -> x == y

instance Eq I64 where
  (I64 x) == (I64 y) =
    case x == minBound of
         True -> False
         False -> case y == minBound of
                       True -> False
                       False -> x == y

instance Show I64 where
  show y@(I64 x) = case isNull y of
                True -> "<null>"
                False -> show x

newtype instance M.MVector s I64 = MV_I64 (M.MVector s (I64, I64))
