module Game
    ( Game(..)
    , Next(..)
    , getOptions
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

getOptions :: Game g => g -> Position g -> [Action g]
getOptions g pos = case next g pos of
  End{}          -> []
  Options _ acts -> acts
