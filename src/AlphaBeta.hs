module AlphaBeta
    ( AlphaBeta(..)
    , OrderingRule(..)
    ) where

import Control.Monad (foldM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Control.Monad.Except as Except
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(..))

import Game
    ( Action
    , Game
    , GameValue
    , Next(..)
    , Position
    , gameValue
    , makeMove
    , next
    , utility
    )
import Player (PlayerMap)
import qualified Player
import Runnable (Runnable, runProxy)
import Strategy (Strategy(..))

newtype OrderingRule g m = OrderingRule ([Action g] -> m [Action g])
newtype AlphaBeta g m = AlphaBeta {mkOrderingRule :: forall s. m s (OrderingRule g (m s))}

data Selection g = Selection (Maybe (Action g)) GameValue

type PValues = PlayerMap GameValue

startPValues :: PValues
startPValues = Player.fromList
  [ (Player.Left, gameValue Player.Left (-1))
  , (Player.Right, gameValue Player.Right (-1))
  ]

instance (Game g, Runnable m) => Strategy (AlphaBeta g m) g where
  decideMoves g AlphaBeta{mkOrderingRule} pos = runProxy $ \(Proxy :: Proxy t) -> do
    orderingRule <- mkOrderingRule
    Selection (fromJust -> act) _ <- posValue g orderingRule startPValues pos
    return [act]

posValue :: forall g m. (Monad m, Game g)
  => g -> OrderingRule g m -> PValues -> Position g -> m (Selection g)
posValue g (OrderingRule orderIt) = go
  where
    go :: PValues -> Position g -> m (Selection g)
    go bnds0 pos = case next g pos of
      Options player acts -> do
        let cutOff = bnds0 Player.! Player.opposite player
            lowBound = bnds0 Player.! player
        ordered <- orderIt acts
        let
          search :: Selection g -> Action g -> ExceptT (Selection g) m (Selection g)
          search cur@(Selection _ curBest) act = do
            let newBnds = Player.insert player curBest bnds0
            Selection _ result <- Except.lift $ go newBnds (makeMove g pos act)
            let newResult = Selection (Just act) result
            if
              | utility player result >= utility player cutOff -> throwError newResult
              | utility player result > utility player curBest -> return newResult
              | otherwise                                      -> return cur
        either id id <$> runExceptT (foldM search (Selection Nothing lowBound) ordered)
      End v               -> return $ Selection Nothing v
