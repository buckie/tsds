
module Tsds.SQL where

import           BasePrelude

import           Tsds.Prim.Types
import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed as U

type ColsClause  = [String]

select :: ColsClause -> Table -> Table
select (n:ns) table =
  case ns of
      [] -> col : []
      _  -> col : select ns table
  where
    col = case lookup n table of
               Nothing -> error "column not found"
               Just x  -> (n,x)
select [] _ = error "no columns given"


--- where clause ---

-- wrapWhere :: (t -> Bool) -> S.IntSet -> S.Key -> Maybe t -> S.IntSet
-- wrapWhere fn = \acc idx value -> case value of
--                                       Nothing -> acc
--                                       Just v -> if fn v
--                                                    then S.insert idx acc
--                                                    else acc
-- class Col a where
--   whereIdx :: (a -> Bool) -> Column -> S.IntSet

-- instance Col Int32_Col where
--   whereIdx fn (Int32_Col v) = U.ifoldl' (wrapWhere fn) S.empty v

-- instance Col Int64_Col where
--   whereIdx fn (Int64_Col v) = U.ifoldl' (wrapWhere fn) S.empty v

wrapWhere :: (t -> Bool) -> S.IntSet -> S.Key -> Maybe t -> S.IntSet
wrapWhere fn = \acc idx value -> case value of
                                      Nothing -> acc
                                      Just v -> if fn v
                                                   then S.insert idx acc
                                                   else acc
whereIdx :: (a -> Bool) -> Column a -> S.IntSet
whereIdx fn (Int32_Col v) = U.ifoldl' (wrapWhere fn) S.empty v
whereIdx fn (Int64_Col v) = U.ifoldl' (wrapWhere fn) S.empty v

-- src/Tsds/SQL.hs|34 col 11 error|
--      Couldn't match type ‘Int32’ with ‘Int64’
--      Expected type: Int32 -> Bool
--      Actual type: Int64 -> Bool
--      In the pattern: fn :: Int64 -> Bool
--      In an equation for ‘whereIdx’: whereIdx (fn :: Int64 -> Bool) (Int64_Col v) = U.ifoldl' (wrapWhere fn) S.empty v
