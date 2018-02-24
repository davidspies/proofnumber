module Game
    ( Game(..)
    , Next(..)
    ) where

import Game.Value
import Player (Player(..))

data Next g = Options Player [Action g] | End GameValue

class Game g where
  data Action g
  data Position g
  next :: g -> Position g -> Next g
  makeMove :: g -> Position g -> Action g -> Position g
  start :: g -> Position g
