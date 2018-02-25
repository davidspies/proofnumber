{-# OPTIONS_GHC -Wno-orphans #-}

module TicTacToe.Display() where

import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.String.Utils (replace)
import Prelude hiding (Either(..))
import Text.Printf (PrintfType, printf)

import Game (Game(..), Next(End, Options))
import Game.Display (Display(..))
import qualified Game.Value as GameValue
import Player (Player(Left, Right))
import TicTacToe.Internal

gameOver :: Position TicTacToe -> Bool
gameOver pos = case next TicTacToe pos of
  End{}     -> True
  Options{} -> False

printfl :: String -> [String] -> String
printfl tmpl = applyArgs (printf tmpl)
  where
    applyArgs :: (forall r. PrintfType r => String -> r) -> [String] -> String
    applyArgs _ []       = error "Empty list"
    applyArgs f [x]      = f x
    applyArgs f (x : xs) = applyArgs (f x) xs

instance Display TicTacToe where
  displayPosition TicTacToe pos@(Position turn pmap) = unlines $
      printfl template (zipWith pieceAt [(1 :: Int)..] allPlaces) :
      ["Turn: " ++ show turn | not (gameOver pos)]
    where
      pieceAt i place = case Map.lookup place pmap of
        Nothing -> show i
        Just p  -> show p
  actionMap TicTacToe (Position _ pmap) =
      catMaybes $ zipWith makeOption [(1 :: Int)..] allPlaces
    where
      makeOption :: Int -> Place -> Maybe (String, Action TicTacToe)
      makeOption i p
        | p `Map.member` pmap = Nothing
        | otherwise = Just (show i, Go p)
  displayGameValue TicTacToe gv
    | GameValue.utility Left gv == 1 = "X wins"
    | GameValue.utility Right gv == 1 = "O wins"
    | GameValue.utility Left gv == 0 = "Draw game"
    | otherwise = show gv

template :: String
template = replace "?" "%v" $ unlines
  [ " ? | ? | ? "
  , "---+---+---"
  , " ? | ? | ? "
  , "---+---+---"
  , " ? | ? | ? "
  ]
