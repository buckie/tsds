
module Tsds.Prim.Types where

import BasePrelude
import Data.Int ()
import Data.Vector.Unboxed.Deriving

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

derivingUnbox "I64"
  [t| I64 -> Int64 |]
  [| \(I64 x) -> x |]
  [| I64 |]
