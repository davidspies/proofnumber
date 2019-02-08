module Minimax
    ( Minimax(..)
    ) where

import DSpies.Prelude

import Game (Game, Next(..), makeMove, next)
import Game.Value (utility)
import Strategy (Strategy(..))

data Minimax g = Minimax

instance Game g => Strategy (Minimax g) g where
  evaluate g Minimax = go
    where
      go pos = case next g pos of
        Options player acts ->
          let actValues = map (go . makeMove g pos) acts
          in maximumOn (utility player) actValues
        End v               -> v

maximumOn :: (HasCallStack, Ord b) => (a -> b) -> [a] -> a
maximumOn f xs = head [x | (x, m) <- zip xs applied, m == maxValue]
  where
    applied = map f xs
    maxValue = maximum applied
