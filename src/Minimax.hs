module Minimax
    ( Minimax(..)
    ) where

import GHC.Stack (HasCallStack)

import Game (Game, GameValue, Next(..), Position, makeMove, next, utility)
import Strategy (Strategy(..))

data Minimax g = Minimax

instance Game g => Strategy (Minimax g) g where
  decideMoves g Minimax pos = case next g pos of
    Options player acts ->
      let actValues = map (utility player . posValue g . makeMove g pos) acts
          maxUtility = maximum actValues
      in [a | (a, u) <- zip acts actValues, u == maxUtility]
    End _ -> []

posValue :: Game g => g -> Position g -> GameValue
posValue g pos = case next g pos of
  Options player acts ->
    let actValues = map (posValue g . makeMove g pos) acts
    in maximumOn (utility player) actValues
  End v               -> v

maximumOn :: (HasCallStack, Ord b) => (a -> b) -> [a] -> a
maximumOn f xs = head [x | (x, m) <- zip xs applied, m == maxValue]
  where
    applied = map f xs
    maxValue = maximum applied
