
module Tsds.Prim.Types where

import           BasePrelude
import           Tsds.Prim.Unbox
import           Data.Vector.Unboxed

type Date = Int32
type DTime = Int64

data Column = Int32_Col (Vector (Maybe Int32))
            | Int64_Col (Vector (Maybe Int64))
            deriving (Show, Read, Eq, Generic)

type Name = String

type Table = [(String, Column)]

