module TicTacToe
    ( TicTacToe(..)
    ) where

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.String.Utils (replace)
import Prelude hiding (Either(..))
import Text.Printf (PrintfType, printf)

import Game (Game(..), Next(End, Options))
import Game.Display (Display(..))
import Game.Value (gameValue, zeroGame)
import qualified Game.Value as GameValue
import Player (Player(Left, Right))

data TicTacToe = TicTacToe

data Place = Place{row :: Int, col :: Int}
  deriving (Eq, Ord, Show)
data Piece = X | O
  deriving (Eq, Enum, Bounded, Show)

opposite :: Piece -> Piece
opposite = \case
  X -> O
  O -> X

allPlaces :: [Place]
allPlaces = [Place{row, col} | row <- [1..3], col <- [1..3]]

allPieces :: [Piece]
allPieces = [minBound..maxBound]

taken :: Place -> Position TicTacToe -> Bool
taken x (Position _ m) = x `Map.member` m

gameOver :: Position TicTacToe -> Bool
gameOver pos = case next TicTacToe pos of
  End{}     -> True
  Options{} -> False

winPatterns :: [[Place]]
winPatterns =
  [[Place{row, col} | row <- [1..3]] | col <- [1..3]] ++
  [[Place{row, col} | col <- [1..3]] | row <- [1..3]] ++
  [[Place{row, col} | (row, col) <- zip [1..3] [1..3]]] ++
  [[Place{row, col} | (row, col) <- zip [1..3] [3,2,1]]]

winner :: Position TicTacToe -> Maybe Piece
winner (Position _ pos) = find (\p -> any (hasWinOn p) winPatterns) allPieces
  where
    hasWinOn :: Piece -> [Place] -> Bool
    hasWinOn p places = all (== Just p) [Map.lookup x pos | x <- places]

whosTurn :: Position TicTacToe -> Piece
whosTurn (Position t _) = t

playerFor :: Piece -> Player
playerFor = \case
  X -> Left
  O -> Right

instance Game TicTacToe where
  data Position TicTacToe = Position Piece (Map Place Piece)
    deriving (Show)
  newtype Action TicTacToe = Go Place
    deriving (Show)
  next TicTacToe pos = case winner pos of
    Just p  -> End $ gameValue (playerFor p) 1
    Nothing ->
      let acts = [Go p | p <- allPlaces, not $ taken p pos]
      in case acts of
        []    -> End zeroGame
        _ : _ -> Options (playerFor $ whosTurn pos) acts

  makeMove TicTacToe (Position p m) (Go place) =
    Position (opposite p) (Map.insert place p m)
  start TicTacToe = Position X Map.empty

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
