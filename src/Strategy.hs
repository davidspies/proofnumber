module Strategy
    ( Strategy(..)
    ) where

import Game (Game, Position)
import Game.Value (GameValue)

class Game g => Strategy s g | s -> g where
  evaluate :: g -> s -> Position g -> GameValue
