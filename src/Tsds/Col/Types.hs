
module Tsds.Col.Types where

import           BasePrelude
import           Data.Default
import           Data.Int                     ()
import           Data.Vector.Unboxed
import           Data.Vector.Unboxed.Deriving

derivingUnbox "Maybe"
    [t| forall a. (Default a, Unbox a) => Maybe a -> (Bool, a) |]
    [| maybe (False, def) (\ x -> (True, x)) |]
    [| \ (b, x) -> if b then Just x else Nothing |]

