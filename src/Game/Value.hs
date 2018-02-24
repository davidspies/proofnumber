module Game.Value
    ( GameValue
    , gameValue
    , utility
    , zeroGame
    ) where

import Data.Function.Pointless ((.:))
import Prelude hiding (Either(..))

import Game.Value.Internal
import Player (Player(..))

gameValue :: Player -> Double -> GameValue
gameValue = GameValue .: \case
  Left -> id
  Right -> negate

utility :: Player -> GameValue -> Double
utility = \case
  Left -> leftUtility
  Right -> negate . leftUtility

zeroGame :: GameValue
zeroGame = GameValue 0
