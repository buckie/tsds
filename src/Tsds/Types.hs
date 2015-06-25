{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}


module Tsds.Types where


import qualified Data.Vector as V
import qualified Data.Text as T

data Col a where
  Lit :: forall a b f. (Functor f, Eq b, Show b, a ~ (V.Vector (Maybe b))) => f b -> Col a


