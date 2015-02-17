
module Tsds.Prim.Types where

import BasePrelude hiding (toList)
import           Data.Vector.Unboxed hiding (replicate)
import           Tsds.Prim.Unbox ()

import qualified Text.PrettyPrint.Boxes as B

-- data Column = Int32_Col (Vector (Maybe Int32))
--             | Int64_Col (Vector (Maybe Int64))
--             deriving (Show, Read, Eq, Generic)

-- data family Column a
-- data instance Column

type ColName = String

type Table = [(ColName, Column)]

printCell :: Show a => Maybe a -> String
printCell (Just c) = show c
printCell Nothing = "<null>"

printCol :: (ColName, Column) -> B.Box
printCol (name, vals) = B.punctuateV B.center1 (B.text $ replicate (2+ B.cols col) '-') (header : [col])
  where
    header = B.text name
    col = B.vcat B.center1 $ B.text `fmap` lst vals
      where
        lst (Int32_Col v) = fmap printCell $ toList v
        lst (Int64_Col v) = fmap printCell $ toList v

printTable :: Table -> IO ()
printTable table = B.printBox $ B.hsep 3 B.center1 $ printCol `fmap` table

sample_table :: Table
sample_table = [ ("foo", v)
               , ("bar", w)
               , ("baz", x)
               , ("bing", y)
               ]
  where
    v = Int32_Col $! fromList [Just 1, Just 2, Just 3, Nothing]
    w = Int32_Col $! fromList [Just 1, Just 2, Nothing, Just 4]
    x = Int32_Col $! fromList [Just 1, Just 2, Nothing, Just 4]
    y = Int32_Col $! fromList [Nothing, Just 2, Just 3, Just 4]

