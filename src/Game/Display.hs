module Game.Display
    ( Display(..)
    ) where

import Game
import SelfTyped (SelfTyped)

class Game g => Display g where
  displayPosition :: g -> Position g -> String
  displayAction :: g -> SelfTyped (Position g) p -> Action g p -> String
