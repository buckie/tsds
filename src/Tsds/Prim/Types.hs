
module Tsds.Prim.Types where

import BasePrelude hiding (toList)
import qualified Data.Vector.Unboxed as U
import           Tsds.Prim.Unbox ()
import qualified Data.IntSet as S
import qualified Text.PrettyPrint.Boxes as B
import           Data.Default

class Col a where
  whereIdx :: (a ~ U.Vector (Maybe b), Col a) => (b -> Bool) -> a -> S.IntSet

-- instance Col (U.Vector (Maybe Int32)) where
--   whereIdx fn v = U.ifoldl' (wrapWhere fn) S.empty v

-- instance Col (U.Vector (Maybe Int64)) where
--   whereIdx fn v = U.ifoldl' (wrapWhere fn) S.empty v

type Int32_Col = U.Vector (Maybe Int32)
type Int64_Col = U.Vector (Maybe Int64)

instance Col Int32_Col where
  whereIdx fn v = U.ifoldl' (wrapWhere fn) S.empty v

instance Col Int64_Col where
  whereIdx fn v = U.ifoldl' (wrapWhere fn) S.empty v

data Column where
  WrapCol :: ( Col a
             , a ~ U.Vector (Maybe b)
             , U.Unbox b
             , Default b
             , Show b
             , Eq b)
             => a -> Column

instance Show Column where
  show (WrapCol v) = show v

wrapWhere :: (t -> Bool) -> S.IntSet -> S.Key -> Maybe t -> S.IntSet
wrapWhere fn = \acc idx value -> case value of
                                      Nothing -> acc
                                      Just v -> if fn v
                                                   then S.insert idx acc
                                                   else acc



-- data Column = Int32_Col (U.Vector (Maybe Int32))
--             | Int64_Col (U.Vector (Maybe Int64))



-- whereIdx :: (a -> Bool) -> Column -> S.IntSet
-- whereIdx (fn:: Int32 -> Bool) (Int32_Col v) = U.ifoldl' (wrapWhere fn) S.empty v
-- whereIdx (fn:: Int64 -> Bool) (Int64_Col v) = U.ifoldl' (wrapWhere fn) S.empty v
-- whereIdx _ _ = error "type mismatch"

-- wrapWhere :: (t -> Bool) -> S.IntSet -> S.Key -> Maybe t -> S.IntSet
-- wrapWhere fn = \acc idx value -> case value of
--                                       Nothing -> acc
--                                       Just v -> if fn v
--                                                    then S.insert idx acc
--                                                    else acc

-- class Col b where
--   whereIdx :: (Col b, Column a ~ b) => (a -> Bool) -> b -> S.IntSet

-- data family Column a
-- data instance Column Int32 = Int32_Col (U.Vector (Maybe Int32))
-- data instance Column Int64 = Int64_Col (U.Vector (Maybe Int64))

-- instance Col (Column Int32) where
--   whereIdx fn (Int32_Col v) = U.ifoldl' (wrapWhere fn) S.empty v

-- instance Col (Column Int64) where
--   whereIdx fn (Int64_Col v) = U.ifoldl' (wrapWhere fn) S.empty v

-- wrapWhere :: (t -> Bool) -> S.IntSet -> S.Key -> Maybe t -> S.IntSet
-- wrapWhere fn = \acc idx value -> case value of
--                                       Nothing -> acc
--                                       Just v -> if fn v
--                                                    then S.insert idx acc
--                                                    else acc

type ColName = String

type Table = [(ColName, Column)]

printCell :: Show a => Maybe a -> String
printCell (Just c) = show c
printCell Nothing = "<null>"

printCol :: (ColName, Column) -> B.Box
printCol (name, (WrapCol vals)) = B.punctuateV B.center1 (B.text $ replicate (2+ B.cols col) '-') (header : [col])
  where
    header = B.text name
    col = B.vcat B.center1 $ B.text `fmap` lst vals
      where
        lst v = fmap printCell $ U.toList v

printTable :: Table -> IO ()
printTable table = B.printBox $ B.hsep 3 B.center1 $ printCol `fmap` table

sample_table :: Table
sample_table = [ ("foo", v)
               , ("bar", w)
               , ("baz", x)
               , ("bing", y)
               ]
  where
    v = WrapCol $! (U.fromList [Just 1, Just 2, Just 3, Nothing] :: Int32_Col) --U.Vector (Maybe Int32))
    w = WrapCol $! (U.fromList [Just 1, Just 2, Nothing, Just 4] :: Int32_Col) --U.Vector (Maybe Int32))
    x = WrapCol $! (U.fromList [Just 1, Just 2, Nothing, Just 4] :: Int32_Col) --U.Vector (Maybe Int32))
    y = WrapCol $! (U.fromList [Nothing, Just 2, Just 3, Just 4] :: Int32_Col) --U.Vector (Maybe Int32))

