
module Tsds.SQL where

import           BasePrelude

import           Tsds.Prim.Types



select :: [String] -> Table -> Table
select (n:ns) table =
  case ns of
      [] -> col : []
      _  -> col : select ns table
  where
    col = case lookup n table of
               Nothing -> error "column not found"
               Just x  -> (n,x)
select [] _ = error "no columns given"



