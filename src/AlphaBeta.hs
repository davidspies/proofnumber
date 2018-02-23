module AlphaBeta
    ( AlphaBeta(..)
    ) where

import Control.Monad (foldM)
import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State
import Data.Maybe (fromJust)

import Game
    (Action, Game, GameValue, Next(..), Position, makeMove, next, utility)
import Player (PlayerMap)
import qualified Player
import SelfTyped (SelfTyped, SomeSelfTyped(..), selfTyped)
import Strategy (Strategy(..))

data AlphaBeta g = forall s. Strategy s g => AlphaBeta {orderingRule :: s}

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y
  | f y > f x = y
  | otherwise = x

instance Game g => Strategy (AlphaBeta g) g where
  decideMoves g ab@AlphaBeta{orderingRule} (pos :: SelfTyped (Position g) s) = case next g pos of
    Options player _ ->
      let
        ordered = decideMoves g orderingRule pos
        go :: Action g s -> State (Maybe GameValue) GameValue
        go act = do
          gv <- State.get
          let pm = Player.fromList [(player, gv), (Player.opposite player, Nothing)]
              newValue = posValue g ab pm $ makeMove g pos act
          State.modify (maxOn (fmap (utility player)) (Just newValue))
          return newValue
        (moveValues, fromJust -> maxv) = runState (mapM go ordered) Nothing
      in [act | (act, v) <- zip ordered moveValues, v == maxv]
    End _            -> []

infty :: Double
infty = read "Infinity"

type PValues = PlayerMap (Maybe GameValue)

posValue :: forall g. Game g
  => g -> AlphaBeta g -> PValues -> Position g -> GameValue
posValue g ab@AlphaBeta{orderingRule} bnds0
    (selfTyped -> SomeSelfTyped (pos :: SelfTyped (Position g) s)) = case next g pos of
  Options player _ ->
    let
      ordered = decideMoves g orderingRule pos
      go :: PValues -> Action g s -> Either PValues PValues
      go bnds act = do
        let result = posValue g ab bnds $ makeMove g pos act
            newBnds = Player.insert player (Just result) bnds
        if
          | utility player result > maybe infty (utility player) (
                bnds Player.! Player.opposite player) ->
              Left newBnds
          | utility player result > maybe (-infty) (utility player) (bnds Player.! player) ->
              Right newBnds
          | otherwise -> Right bnds
      newbnds = either id id $ foldM go bnds0 ordered
    in fromJust $ newbnds Player.! player
  End v            -> v
