module Game.Display
    ( Display(..)
    , stringyMove
    ) where

import Game
import SelfTyped (SelfTyped, SomeSelfTyped(..), selfTyped)

class Game g => Display g where
  displayPosition :: g -> Position g -> String
  displayAction :: g -> SelfTyped (Position g) p -> Action g p -> String
  readAction :: g -> SelfTyped(Position g) p -> String -> Action g p

stringyMove :: Display g => g -> Position g -> String -> Position g
stringyMove g pos moveString = case selfTyped pos of
  SomeSelfTyped p -> makeMove g p (readAction g p moveString)
