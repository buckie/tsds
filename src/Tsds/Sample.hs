

module Tsds.Sample where

import Tsds.Types
import Data.Vector
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)
import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set

type Id        = Int
type FirstName = String
type LastName  = String

names :: Map Id (FirstName, LastName)
names = Map.fromList
    [ (0, ("Gabriel", "Gonzalez"))
    , (1, ("Oscar"  , "Boykin"  ))
    , (2, ("Edgar"  , "Codd"    ))
    ]

type TwitterHandle = String

handles :: Map Id TwitterHandle
handles = Map.fromList
    [ (0, "GabrielG439")
    , (1, "posco"      )
    , (3, "avibryant"  )
    ]

names' :: Table Id (FirstName, LastName)
names' = from names

handles' :: Table Id TwitterHandle
handles' = from handles

