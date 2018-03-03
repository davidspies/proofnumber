{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module DotsAndBoxes.Internal
    ( DotsAndBoxes(..)
    , Direction(..)
    , Edge(..)
    , Action(..)
    , Position(..)
    , allPositions
    ) where

import Data.Hashable (Hashable)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

import Game (Game(..), Next(..))
import Game.Value (GameValue, win, zeroGame)
import Orphans ()
import Player (Player)
import qualified Player

data Direction = V | H
  deriving (Eq, Ord, Generic, Hashable)
data DotsAndBoxes = DotsAndBoxes {nrows :: Int, ncols :: Int}
data Edge = Edge{row :: Int, col :: Int, dir :: Direction}
  deriving (Eq, Ord, Generic, Hashable)

allPositions :: DotsAndBoxes -> [Edge]
allPositions DotsAndBoxes{nrows, ncols} =
  [Edge{row, col, dir=H} | row <- [0..nrows], col <- [1..ncols]] ++
  [Edge{row, col, dir=V} | row <- [1..nrows], col <- [0..ncols]]

boxesCompleted :: Edge -> Set Edge -> Int
boxesCompleted Edge{row, col, dir} es = case dir of
  H -> sum $
    [1 | allMembers
      [Edge row (col - 1) V, Edge row col V, Edge (row - 1) col H]] ++
    [1 | allMembers
      [Edge (row + 1) (col - 1) V, Edge (row + 1) col V, Edge (row + 1) col H]]
  V -> sum $
    [1 | allMembers
      [Edge (row - 1) col H, Edge row col H, Edge row (col - 1) V]] ++
    [1 | allMembers
      [Edge (row - 1) (col + 1) H, Edge row (col + 1) H, Edge row (col + 1) V]]
  where
    allMembers = all (`Set.member` es)

hasEdge :: Position DotsAndBoxes -> Edge -> Bool
hasEdge Position{edges} = (`Set.member` edges)

score :: Position DotsAndBoxes -> GameValue
score Position{..} = case compare leftScore rightScore of
  LT -> win Player.Right
  EQ -> zeroGame
  GT -> win Player.Left

instance Game DotsAndBoxes where
  newtype Action DotsAndBoxes = Action Edge
  data Position DotsAndBoxes = Position
    { edges :: Set Edge
    , leftScore :: Int
    , rightScore :: Int
    , turn :: Player
    }
    deriving (Eq, Generic, Hashable)

  next g = go
    where
      allpos = allPositions g
      nextActions pos = map Action $ filter (not . hasEdge pos) allpos
      go pos@Position{turn} = case nextActions pos of
        []           -> End $ score pos
        acts@(_ : _) -> Options turn acts
  start _ = Position
    { edges = Set.empty
    , leftScore = 0
    , rightScore = 0
    , turn = Player.Left
    }
  makeMove _ Position{..} (Action e) = Position
    { edges = Set.insert e edges
    , leftScore = newLeft
    , rightScore = newRight
    , turn = if cmpd > 0 then turn else Player.opposite turn
    }
    where
      cmpd = boxesCompleted e edges
      (newLeft, newRight) = case turn of
        Player.Left  -> (leftScore + cmpd, rightScore)
        Player.Right -> (leftScore, rightScore + cmpd)
