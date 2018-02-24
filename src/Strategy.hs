module Strategy
    ( Strategy(..)
    ) where

import Game (Action, Game, Next(..), Position, makeMove, next)
import Game.Value (GameValue, utility)

class Game g => Strategy s g | s -> g where
  evaluate :: g -> s -> Position g -> GameValue
  decideMoves :: g -> s -> Position g -> [Action g]
  decideMoves g s pos = case next g pos of
    End _               -> []
    Options player opts ->
      let optvalues = map (utility player . evaluate g s . makeMove g pos) opts
          maxv = maximum optvalues
      in [o | (o, v) <- zip opts optvalues, v == maxv]
