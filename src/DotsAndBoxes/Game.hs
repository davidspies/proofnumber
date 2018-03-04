{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DotsAndBoxes.Game
    ( DotsAndBoxes(..)
    , Direction(..)
    , Edge(..)
    , GridSize(..)
    , Action(..)
    , Position(..)
    , allPositions
    ) where

import Data.Hashable (Hashable)
import Data.Proxy (Proxy(Proxy))
import Data.Reflection (Reifies, reflect)
import GHC.Generics (Generic)

import DotsAndBoxes.EdgeVector (EdgeVector)
import qualified DotsAndBoxes.EdgeVector as EdgeVector
import DotsAndBoxes.Internal
import Game (Game(..), Next(..))
import Game.Value (GameValue, gameValue)
import Orphans ()
import Player (Player)
import qualified Player

data DotsAndBoxes gs = DotsAndBoxes

allPositions :: forall gs. Reifies gs GridSize => [Edge gs]
allPositions =
  let GridSize{nrows, ncols} = reflect (Proxy :: Proxy gs) in
  [Edge{row, col, dir=H} | row <- [0..nrows], col <- [1..ncols]] ++
  [Edge{row, col, dir=V} | row <- [1..nrows], col <- [0..ncols]]

boxesCompleted :: Reifies gs GridSize => Edge gs -> EdgeVector gs -> Int
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
    allMembers = all (`EdgeVector.member` es)

hasEdge :: Reifies gs GridSize => Position (DotsAndBoxes gs) -> Edge gs -> Bool
hasEdge Position{edges} = (`EdgeVector.member` edges)

score :: forall gs. Reifies gs GridSize
  => Position (DotsAndBoxes gs) -> GameValue
score Position{..} =
    gameValue Player.Left $ fromIntegral (leftScore - rightScore)

instance Reifies gs GridSize => Game (DotsAndBoxes gs) where
  newtype Action (DotsAndBoxes gs) = Action (Edge gs)
  data Position (DotsAndBoxes gs) = Position
    { edges :: EdgeVector gs
    , leftScore :: Int
    , rightScore :: Int
    , turn :: Player
    }
    deriving (Eq, Generic, Hashable)

  next _ = go
    where
      allpos = allPositions
      nextActions pos = map Action $ filter (not . hasEdge pos) allpos
      go pos@Position{turn} = case nextActions pos of
        []           -> End $ score pos
        acts@(_ : _) -> Options turn acts
  start _ = Position
    { edges = EdgeVector.empty
    , leftScore = 0
    , rightScore = 0
    , turn = Player.Left
    }
  makeMove _ Position{..} (Action e) = Position
    { edges = EdgeVector.insert e edges
    , leftScore = newLeft
    , rightScore = newRight
    , turn = if cmpd > 0 then turn else Player.opposite turn
    }
    where
      cmpd = boxesCompleted e edges
      (newLeft, newRight) = case turn of
        Player.Left  -> (leftScore + cmpd, rightScore)
        Player.Right -> (leftScore, rightScore + cmpd)
