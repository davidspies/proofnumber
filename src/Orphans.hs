{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans () where

import Data.Hashable (Hashable(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

instance (Hashable k, Hashable v) => Hashable (Map k v) where
  hashWithSalt salt m = hashWithSalt salt $ Map.toList m
instance (Hashable k) => Hashable (Set k) where
  hashWithSalt salt s = hashWithSalt salt $ Set.toList s
