
module Tsds.Table.Types where

import BasePrelude

import Tsds.Col.Types
import Tsds.Prim.Types

data Table = Table [Col]
