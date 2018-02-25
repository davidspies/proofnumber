module AlphaBeta
    ( AlphaBeta(..)
    , OrderingRule
    ) where

import Control.Monad (foldM)
import Data.DList (DList)
import qualified Data.DList as DList

import Game (Action, Game, Next(..), Position, makeMove, next)
import Game.Value (GameValue)
import qualified Game.Value as GameValue
import Player (PlayerMap)
import qualified Player
import Strategy (Strategy(..))

type OrderingRule g = Position g -> [Action g]
newtype AlphaBeta g = AlphaBeta{orderingRule :: OrderingRule g}

data Selection g = Selection (DList (Action g)) GameValue

type PValues = PlayerMap GameValue

startPValues :: PValues
startPValues = Player.fromList
  [ (Player.Left, GameValue.lose Player.Left)
  , (Player.Right, GameValue.lose Player.Right)
  ]

instance Game g => Strategy (AlphaBeta g) g where
  evaluate g ab pos =
    let Selection _ v = evSelection g ab pos in v
  decideMoves g ab pos =
    let Selection acts _ = evSelection g ab pos in DList.toList acts

evSelection :: Game g => g -> AlphaBeta g -> Position g -> Selection g
evSelection g AlphaBeta{orderingRule} = posValue g orderingRule

posValue :: forall g. (Game g)
  => g -> OrderingRule g -> Position g -> Selection g
posValue g orderIt = go startPValues
  where
    go :: PValues -> Position g -> Selection g
    go bnds0 pos = case next g pos of
      Options player _ ->
        let
          cutOff = bnds0 Player.! Player.opposite player
          ordered = orderIt pos
          startSelection = Selection DList.empty (GameValue.lose player)
          search
            :: Selection g -> Action g -> Either (Selection g) (Selection g)
          search cur@(Selection curActs curBest) act =
            let
              newBnds = Player.insert player curBest bnds0
              Selection _ result = go newBnds (makeMove g pos act)
              newResult = Selection (DList.singleton act) result
              playerUtil = GameValue.utility player
            in if playerUtil result > playerUtil cutOff
              then Left newResult
              else Right $
                case compare (playerUtil result) (playerUtil curBest) of
                  LT -> cur
                  EQ -> Selection (curActs `DList.snoc` act) curBest
                  GT -> newResult
        in either id id (foldM search startSelection ordered)
      End v -> Selection DList.empty v
