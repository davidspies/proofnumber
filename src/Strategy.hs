module Strategy
    ( Strategy(..)
    ) where

import Game (Action, Game, Position)

class Game g => Strategy s g | s -> g where
  decideMoves :: g -> s -> Position g -> [Action g]
