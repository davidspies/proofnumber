module Misere
    ( Misere(..)
    ) where

import Data.Hashable (Hashable)
import Prelude hiding (negate)

import Game (Game(..), Next(..))
import Game.Display (Display)
import Game.Value.Internal (GameValue(..))

negate :: GameValue -> GameValue
negate (GameValue l) = GameValue (-l)

newtype Misere g = Misere g
  deriving (Display)

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
deriving instance Hashable (Position g) => Hashable (Position (Misere g))
deriving instance Show (Position g) => Show (Position (Misere g))
