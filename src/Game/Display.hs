module Game.Display
    ( Display(..)
    ) where

import Game

class Game g => Display g where
  displayPosition :: g -> Position g -> String
  displayAction :: g -> Position g -> Action g -> String
  readAction :: g -> Position g -> String -> Action g
