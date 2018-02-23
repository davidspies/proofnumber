module Main where

import qualified System.Random.PCG as Rand

import AlphaBeta
import qualified AlphaBeta.Ordering as Ordering
import Game
import Strategy
import TicTacToe

main :: IO ()
main = do
  seed <- Rand.createSystemRandom >>= Rand.save
  print $ decideMoves TicTacToe (AlphaBeta $ Ordering.random seed) (start TicTacToe)
