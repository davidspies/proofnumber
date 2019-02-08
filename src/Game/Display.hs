module Game.Display
    ( Display(..)
    ) where

import DSpies.Prelude

import Game
import Game.Value (GameValue)

class Game g => Display g where
  displayPosition :: g -> Position g -> String
  actionMap :: g -> Position g -> [(String, Action g)]
  displayGameValue :: g -> GameValue -> String
  displayGameValue = const show
