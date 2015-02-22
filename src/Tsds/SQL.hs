
module Tsds.SQL where

import           BasePrelude

import           Tsds.Prim.Types
import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed as U

type ColsClause  = [String]

-- select :: ColsClause -> Table -> Table
-- select (n:ns) table =
--   case ns of
--       [] -> col : []
--       _  -> col : select ns table
--   where
--     col = case lookup n table of
--                Nothing -> error "column not found"
--                Just x  -> (n,x)
-- select [] _ = error "no columns given"

