module Tsds.Prim.Types where

import           BasePrelude hiding (toList)

import           Data.Proxy ()

import qualified Text.PrettyPrint.Boxes as B
import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed as VU

import           Tsds.Prim.Unbox ()

class (Show a) => Column a where
  nullShow :: a -> [String]
  whereIdx32 :: (Int32 -> Bool) -> a -> S.IntSet
  whereIdx64 :: (Int64 -> Bool) -> a -> S.IntSet
  getRows :: S.IntSet -> a -> a

data Col = Int32_Col (VU.Vector (Maybe Int32))
         | Int64_Col (VU.Vector (Maybe Int64))
         | Date_Col  (VU.Vector (Maybe Int32))
         | DTime_Col (VU.Vector (Maybe Int64))
         deriving (Read, Show, Eq)

nullShowVal :: (Show a) => Maybe a -> String
nullShowVal (Just v) = show v
nullShowVal _        = "<null>"

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


instance Column Col where
  nullShow (Int32_Col v) = nullShowVal `fmap` (VU.toList v)
  nullShow (Int64_Col v) = nullShowVal `fmap` (VU.toList v)
  nullShow (Date_Col  v) = nullShowVal `fmap` (VU.toList v)
  nullShow (DTime_Col v) = nullShowVal `fmap` (VU.toList v)

  getRows set (Int32_Col v) = Int32_Col $ VU.ifilter (\idx _ -> S.member idx set) v
  getRows set (Int64_Col v) = Int64_Col $ VU.ifilter (\idx _ -> S.member idx set) v
  getRows set (Date_Col  v) = Date_Col $ VU.ifilter (\idx _ -> S.member idx set) v
  getRows set (DTime_Col v) = DTime_Col $ VU.ifilter (\idx _ -> S.member idx set) v

  whereIdx32 fn (Int32_Col v) = VU.ifoldl' (\set idx val -> if maybe False fn val then S.insert idx set else set) S.empty v
  whereIdx32 fn (Date_Col v) = VU.ifoldl' (\set idx val -> if maybe False fn val then S.insert idx set else set) S.empty v
  whereIdx32 _ _ = error "Type Mismatch"

  whereIdx64 fn (Int64_Col v) = VU.ifoldl' (\set idx val -> if maybe False fn val then S.insert idx set else set) S.empty v
  whereIdx64 fn (DTime_Col v) = VU.ifoldl' (\set idx val -> if maybe False fn val then S.insert idx set else set) S.empty v
  whereIdx64 _ _ = error "Type Mismatch"


type Table = [(String, Col)]

printCol :: (String, Col) -> B.Box
printCol (name, vals) = B.punctuateV B.center1 (B.text $ replicate (2+ B.cols col) '-') (header : [col])
  where
    header = B.text name
    col = B.vcat B.center1 $ B.text `fmap` (nullShow vals)

printTable :: Table -> IO ()
printTable table = B.printBox $ B.hsep 3 B.center1 $ printCol `fmap` table

