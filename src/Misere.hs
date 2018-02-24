module Misere
    ( Misere(..)
    ) where

import Prelude hiding (negate)

import Game
import Game.Value.Internal

negate :: GameValue -> GameValue
negate (GameValue l) = GameValue (-l)

newtype Misere g = Misere g

instance Game g => Game (Misere g) where
  newtype Action (Misere g) = MisereAction (Action g)
  newtype Position (Misere g) = MiserePosition (Position g)
  makeMove (Misere g) (MiserePosition p) (MisereAction m) =
    MiserePosition (makeMove g p m)
  next (Misere g) (MiserePosition p) = case next g p of
    End v               -> End $ negate v
    Options player acts -> Options player (map MisereAction acts)
  start (Misere g) = MiserePosition (start g)

deriving instance Show (Action g) => Show (Action (Misere g))
