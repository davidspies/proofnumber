module Game
    ( Game(..)
    , GameValue
    , Next(..)
    , gameValue
    , utility
    , zeroGame
    ) where

import Data.Function.Pointless ((.:))
import Prelude hiding (Either(..))

import Player (Player(..))
import SelfTyped (SelfTyped)

newtype GameValue = GameValue {leftUtility :: Double}
  deriving (Eq, Show)

zeroGame :: GameValue
zeroGame = GameValue 0

data Next g p = Options Player [Action g p] | End GameValue

class Game g where
  data Action g p
  data Position g
  next :: g -> SelfTyped (Position g) p -> Next g p
  makeMove :: g -> SelfTyped (Position g) p -> Action g p -> Position g
  start :: g -> Position g

gameValue :: Player -> Double -> GameValue
gameValue = GameValue .: \case
  Left -> id
  Right -> negate

utility :: Player -> GameValue -> Double
utility = \case
  Left -> leftUtility
  Right -> negate . leftUtility
