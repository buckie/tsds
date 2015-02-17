
module Tsds.Prim.Fam where

import qualified Data.Vector.Unboxed as V
import BasePrelude hiding (index)

import Tsds.Prim.Unbox ()


data family Column a
data instance Column Int32     = Int32_Col (V.Vector (Maybe Int32))
data instance Column Int64     = Int64_Col (V.Vector (Maybe Int64))

type Table = [(String, Column)]
-- class IColumn a where
--   index :: Column a -> Int -> a

-- -- Vector of missing values
-- instance (IColumn a) => IColumn (Maybe a) where
--   index (MColumn bm xs) i =
--     case bm V.! i of
--       True  -> Nothing
--       False -> Just $ index xs i
