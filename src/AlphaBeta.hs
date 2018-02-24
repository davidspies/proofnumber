module AlphaBeta
    ( AlphaBeta(..)
    , OrderingRule(..)
    ) where

import Control.Monad (foldM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Control.Monad.Except as Except
import Data.DList (DList)
import qualified Data.DList as DList

import Game (Action, Game, Next(..), Position, makeMove, next)
import Game.Value (GameValue)
import qualified Game.Value as GameValue
import Player (PlayerMap)
import qualified Player
import Runnable (Runnable)
import qualified Runnable
import Strategy (Strategy(..))

newtype OrderingRule g m = OrderingRule ([Action g] -> m [Action g])
newtype AlphaBeta g m = AlphaBeta
  {mkOrderingRule :: forall s. m s (OrderingRule g (m s))}

data Selection g = Selection (DList (Action g)) GameValue

type PValues = PlayerMap GameValue

startPValues :: PValues
startPValues = Player.fromList
  [ (Player.Left, GameValue.lose Player.Left)
  , (Player.Right, GameValue.lose Player.Right)
  ]

instance (Game g, Runnable m) => Strategy (AlphaBeta g m) g where
  evaluate g ab pos =
    let Selection _ v = evSelection g ab pos in v
  decideMoves g ab pos =
    let Selection acts _ = evSelection g ab pos in DList.toList acts

evSelection :: (Game g, Runnable m)
  => g -> AlphaBeta g m -> Position g -> Selection g
evSelection g AlphaBeta{mkOrderingRule} pos =
  Runnable.run $ do
    orderingRule <- mkOrderingRule
    posValue g orderingRule pos

posValue :: forall g m. (Monad m, Game g)
  => g -> OrderingRule g m -> Position g -> m (Selection g)
posValue g (OrderingRule orderIt) = go startPValues
  where
    go :: PValues -> Position g -> m (Selection g)
    go bnds0 pos = case next g pos of
      Options player acts -> do
        let cutOff = bnds0 Player.! Player.opposite player
        ordered <- orderIt acts
        let
          search
            :: Selection g -> Action g -> ExceptT (Selection g) m (Selection g)
          search cur@(Selection curActs curBest) act = do
            let newBnds = Player.insert player curBest bnds0
            Selection _ result <- Except.lift $ go newBnds (makeMove g pos act)
            let newResult = Selection (DList.singleton act) result
                playerUtil = GameValue.utility player
            if playerUtil result > playerUtil cutOff
              then throwError newResult
              else return $
                case compare (playerUtil result) (playerUtil curBest) of
                  LT -> cur
                  EQ -> Selection (curActs `DList.snoc` act) curBest
                  GT -> newResult
        either id id <$> runExceptT
          (foldM search (Selection DList.empty (GameValue.lose player)) ordered)
      End v               -> return $ Selection DList.empty v
