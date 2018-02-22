module Strategy
    ( Strategy(..)
    ) where

import Game (Action, Game, Position)
import SelfTyped (SelfTyped)

class Game g => Strategy s g | s -> g where
  decideMoves :: g -> s -> SelfTyped (Position g) p -> [Action g p]
