
module Tsds.SQL where

import           BasePrelude

import           Tsds.Prim.Types
import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed as VU

sample_table :: Table
sample_table = [ ("foo", v)
               , ("bar", w)
               , ("baz", x)
               , ("bing", y)
               ]
  where
    v = Int32_Col (VU.fromList [Just 1, Just 2, Just 3, Nothing])
    w = Int32_Col (VU.fromList [Just 1, Just 2, Nothing, Just 4])
    x = Int32_Col (VU.fromList [Just 1, Just 2, Nothing, Just 4])
    y = Int32_Col (VU.fromList [Nothing, Just 2, Just 3, Just 4])

sample_col :: Col
sample_col = Int32_Col (VU.fromList [Just 1, Just 2, Just 3, Nothing])

sample_column :: (String, Col)
sample_column = ("foo", sample_col)

sample_vec :: VU.Vector (Maybe Int32)
sample_vec = VU.fromList [Just 1, Just 2, Just 3, Nothing]

select :: [String] -> Table -> Table
select (n:[]) table = maybe (error "Col Not Found") (\x -> (n,x)) (lookup n table) : []
select (n:ns) table = maybe (error "Col Not Found") (\x -> (n,x)) (lookup n table) : select ns table
select _ _ = error "No Table Given"

selectRows :: Table -> S.IntSet -> Table
selectRows ((cName, vals):[]) set = (cName, getRows set vals) : []
selectRows ((cName, vals):cs) set = (cName, getRows set vals) : selectRows cs set
selectRows [] _ = error "No Table Given"

