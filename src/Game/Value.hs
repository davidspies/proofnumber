module Game.Value
    ( GameValue
    , gameValue
    , lose
    , utility
    , win
    , zeroGame
    ) where

import DSpies.Prelude hiding (Either(..))

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

lose :: Player -> GameValue
lose p = gameValue p (-1)

win :: Player -> GameValue
win p = gameValue p 1

zeroGame :: GameValue
zeroGame = GameValue 0
