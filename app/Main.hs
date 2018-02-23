module Main where

import Game
import Minimax
import SelfTyped
import Some
import Strategy
import TicTacToe

main :: IO ()
main =
  print $ case selfTyped (start TicTacToe) of
    SomeSelfTyped p -> map Some $ decideMoves TicTacToe Minimax p
