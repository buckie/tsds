
module Tsds.Prim.Types where

import           BasePrelude
import           Data.Default
import           Data.Int                     ()
import           Data.Vector.Unboxed
import           Data.Vector.Unboxed.Deriving


type Date = Int32
type DateTime = Int64

derivingUnbox "Maybe"
    [t| forall a. (Default a, Unbox a) => Maybe a -> (Bool, a) |]
    [| maybe (False, def) (\ x -> (True, x)) |]
    [| \ (b, x) -> if b then Just x else Nothing |]


