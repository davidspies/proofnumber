module Main where

import qualified System.Random.PCG as Rand

import AlphaBeta
import qualified AlphaBeta.Ordering as Ordering
import Game
import Misere
import Strategy
import TicTacToe

game :: Misere TicTacToe
game = Misere TicTacToe

main :: IO ()
main = do
  seed <- Rand.createSystemRandom >>= Rand.save
  print $ decideMoves game (AlphaBeta $ Ordering.random seed) (start game)
