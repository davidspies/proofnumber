{-# LANGUAGE DeriveAnyClass #-}

module DotsAndBoxes.Internal
    ( Direction(..)
    , GridSize(..)
    , Edge(..)
    ) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Direction = V | H
  deriving (Eq, Ord, Show, Generic, Hashable)
data GridSize = GridSize {nrows :: Int, ncols :: Int}
data Edge gs = Edge{row :: Int, col :: Int, dir :: Direction}
  deriving (Eq, Ord, Show, Generic, Hashable)
