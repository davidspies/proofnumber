module Main where

import qualified System.Random.PCG as Rand

import AlphaBeta
import qualified AlphaBeta.Ordering as Ordering
import Game
import SelfTyped
import Some
import Strategy
import TicTacToe

main :: IO ()
main = do
  seed <- Rand.createSystemRandom >>= Rand.save
  print $ case selfTyped (start TicTacToe) of
    SomeSelfTyped p -> map Some $ decideMoves TicTacToe (AlphaBeta $ Ordering.random seed) p
