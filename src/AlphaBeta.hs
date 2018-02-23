module AlphaBeta
    ( AlphaBeta(..)
    , OrderingRule(..)
    ) where

import Control.Monad (foldM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Control.Monad.Except as Except
import Control.Monad.State (StateT, runStateT)
import qualified Control.Monad.State as State
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(..))

import Game
    (Action, Game, GameValue, Next(..), Position, makeMove, next, utility)
import Player (PlayerMap)
import qualified Player
import Runnable (Runnable, runProxy)
import Strategy (Strategy(..))

newtype OrderingRule g m = OrderingRule ([Action g] -> m [Action g])
newtype AlphaBeta g m = AlphaBeta {mkOrderingRule :: forall s. m s (OrderingRule g (m s))}

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y
  | f y > f x = y
  | otherwise = x

instance (Game g, Runnable m) => Strategy (AlphaBeta g m) g where
  decideMoves g AlphaBeta{mkOrderingRule} pos = case next g pos of
    Options player acts -> runProxy $ \(Proxy :: Proxy t) -> do
      orderingRule@(OrderingRule r) <- mkOrderingRule
      ordered <- r acts
      let
        search :: Action g -> StateT (Maybe GameValue) (m t) GameValue
        search act = do
          gv <- State.get
          let pm = Player.fromList [(player, gv), (Player.opposite player, Nothing)]
          newValue <- State.lift $ posValue g orderingRule pm $ makeMove g pos act
          State.modify (maxOn (fmap (utility player)) (Just newValue))
          return newValue
      (moveValues, fromJust -> maxv) <- runStateT (mapM search ordered) Nothing
      return [act | (act, v) <- zip ordered moveValues, v == maxv]
    End _            -> []

infty :: Double
infty = read "Infinity"

type PValues = PlayerMap (Maybe GameValue)

posValue :: forall g m. (Monad m, Game g)
  => g -> OrderingRule g m -> PValues -> Position g -> m GameValue
posValue g (OrderingRule orderIt) = go
  where
    go bnds0 pos = case next g pos of
      Options player acts -> do
        ordered <- orderIt acts
        let
          search :: PValues -> Action g -> ExceptT GameValue m PValues
          search bnds act = do
            result <- Except.lift $ go bnds $ makeMove g pos act
            let newBnds = Player.insert player (Just result) bnds
            if
              | utility player result > maybe infty (utility player) (
                    bnds Player.! Player.opposite player) ->
                  throwError result
              | utility player result > maybe (-infty) (utility player) (bnds Player.! player) ->
                  return newBnds
              | otherwise -> return bnds
        either id (fromJust . (Player.! player)) <$> runExceptT (foldM search bnds0 ordered)
      End v               -> return v
