{-# LANGUAGE DeriveAnyClass #-}

module DotsAndBoxes.Internal
    ( Direction(..)
    , GridSize(..)
    , Edge(..)
    ) where

import DSpies.Prelude

import Data.Hashable (Hashable)

data Direction = V | H
  deriving (Eq, Ord, Show, Generic, Hashable)
data GridSize = GridSize {nrows :: Int, ncols :: Int}
data Edge gs = Edge{row :: Int, col :: Int, dir :: Direction}
  deriving (Eq, Ord, Show, Generic, Hashable)
