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

newtype GameValue = GameValue {leftUtility :: Double}
  deriving (Eq, Show)

zeroGame :: GameValue
zeroGame = GameValue 0

data Next g = Options Player [Action g] | End GameValue

class Game g where
  data Action g
  data Position g
  next :: g -> Position g -> Next g
  makeMove :: g -> Position g -> Action g -> Position g
  start :: g -> Position g

gameValue :: Player -> Double -> GameValue
gameValue = GameValue .: \case
  Left -> id
  Right -> negate

utility :: Player -> GameValue -> Double
utility = \case
  Left -> leftUtility
  Right -> negate . leftUtility
