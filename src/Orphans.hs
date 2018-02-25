{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans () where

import Data.Hashable (Hashable(..))
import Data.Map (Map)
import qualified Data.Map as Map

instance (Hashable k, Hashable v) => Hashable (Map k v) where
  hashWithSalt salt m = hashWithSalt salt $ Map.toList m
