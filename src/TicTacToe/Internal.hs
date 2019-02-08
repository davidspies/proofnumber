{-# LANGUAGE DeriveAnyClass #-}

module TicTacToe.Internal
    ( TicTacToe(..)
    , Place
    , Action(..)
    , Position(..)
    , allPlaces
    ) where

import DSpies.Prelude hiding (Either(..))

import Data.Hashable (Hashable)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)

import Game (Game(..), Next(End, Options))
import Game.Value (gameValue, zeroGame)
import Orphans ()
import Player (Player(Left, Right))

data TicTacToe = TicTacToe

data Place = Place{row :: Int, col :: Int}
  deriving (Eq, Generic, Hashable, Ord, Show)
data Piece = X | O
  deriving (Eq, Generic, Hashable, Enum, Bounded, Show)

opposite :: Piece -> Piece
opposite = \case
  X -> O
  O -> X

allPlaces :: [Place]
allPlaces = [Place{row, col} | row <- [1..3], col <- [1..3]]

allPieces :: [Piece]
allPieces = [minBound..maxBound]

taken :: Place -> Position TicTacToe -> Bool
taken x (Position _ m) = x `Map.member` m

winPatterns :: [[Place]]
winPatterns =
  [[Place{row, col} | row <- [1..3]] | col <- [1..3]] ++
  [[Place{row, col} | col <- [1..3]] | row <- [1..3]] ++
  [[Place{row, col} | (row, col) <- zip [1..3] [1..3]]] ++
  [[Place{row, col} | (row, col) <- zip [1..3] [3,2,1]]]

winner :: Position TicTacToe -> Maybe Piece
winner (Position _ pos) = find (\p -> any (hasWinOn p) winPatterns) allPieces
  where
    hasWinOn :: Piece -> [Place] -> Bool
    hasWinOn p places = all (== Just p) [Map.lookup x pos | x <- places]

whosTurn :: Position TicTacToe -> Piece
whosTurn (Position t _) = t

playerFor :: Piece -> Player
playerFor = \case
  X -> Left
  O -> Right

instance Game TicTacToe where
  data Position TicTacToe = Position Piece (Map Place Piece)
    deriving (Generic, Eq, Hashable, Show)
  newtype Action TicTacToe = Go Place
    deriving (Show)
  next TicTacToe pos = case winner pos of
    Just p  -> End $ gameValue (playerFor p) 1
    Nothing ->
      let acts = [Go p | p <- allPlaces, not $ taken p pos]
      in case acts of
        []    -> End zeroGame
        _ : _ -> Options (playerFor $ whosTurn pos) acts

  makeMove TicTacToe (Position p m) (Go place) =
    Position (opposite p) (Map.insert place p m)
  start TicTacToe = Position X Map.empty
