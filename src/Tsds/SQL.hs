
module Tsds.SQL where

import           BasePrelude

import           Tsds.Prim.Types
import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

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

sample_column :: Column
sample_column = ("foo", sample_col)

sample_vec :: VU.Vector (Maybe Int32)
sample_vec = VU.fromList [Just 1, Just 2, Just 3, Nothing]

-- whereIdx ::  forall t . (Typeable t) => (t -> Bool) -> ColStoreExist -> Either String S.IntSet
-- whereIdx fn (CStore v) | VG.null v == True              = Left "empty Column"
--                        | Nothing <- fn' $ v VG.! 0  = Left $ "MisMatched types! Wanted: \""
--                                                               ++ show (typeOf (v VG.! 0))
--                                                               ++ "\" Got: \""
--                                                               ++ show (typeOf (fn)) ++ "\""
--                        | otherwise =  Right $
--                               VG.ifoldl' (\ set idx val ->
--                                           case fn' val of
--                                                Nothing   -> error "wtf"
--                                                Just True    -> S.insert idx set
--                                                _         -> set
--                                          )
--                                          S.empty  v

--               where
--                 fn' :: (Typeable b) => b -> Maybe Bool
--                 fn' = whereWrap fn

-- whereWrap :: (Typeable a1, Typeable a) => (a -> b) -> a1 -> Maybe b
-- whereWrap fn x = fn `fmap` cast x

-- foo :: Ord a => a -> Maybe a -> Bool
-- foo y (Just x) = x > y
-- foo _ Nothing = False

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

