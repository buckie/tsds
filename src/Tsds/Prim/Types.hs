module Tsds.Prim.Types where

import           BasePrelude hiding (toList)

import           Data.Proxy ()

import qualified Text.PrettyPrint.Boxes as B

import qualified Data.Vector.Unboxed as VU

import           Tsds.Prim.Unbox ()

nullShowVal :: (Show a) => Maybe a -> String
nullShowVal (Just v) = show v
nullShowVal _        = "<null>"

data Col = Int32_Col (VU.Vector (Maybe Int32))
         | Int64_Col (VU.Vector (Maybe Int64))
         | Date_Col  (VU.Vector (Maybe Int32))
         | DTime_Col (VU.Vector (Maybe Int64))
         deriving (Read, Show, Eq)

class (Show a) => Nullable a where
  nullShow :: a -> [String]

instance Nullable Col where
  nullShow (Int32_Col v) = nullShowVal `fmap` (VU.toList v)
  nullShow (Int64_Col v) = nullShowVal `fmap` (VU.toList v)
  nullShow (Date_Col v) = nullShowVal `fmap` (VU.toList v)
  nullShow (DTime_Col v) = nullShowVal `fmap` (VU.toList v)

type Column = (String, Col)
type Table = [Column]

printCol :: Column -> B.Box
printCol (name, vals) = B.punctuateV B.center1 (B.text $ replicate (2+ B.cols col) '-') (header : [col])
  where
    header = B.text name
    col = B.vcat B.center1 $ B.text `fmap` (nullShow vals)

printTable :: Table -> IO ()
printTable table = B.printBox $ B.hsep 3 B.center1 $ printCol `fmap` table

