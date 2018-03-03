module AlphaBeta
    ( AlphaBeta(..)
    , OrderingRule
    ) where

import Data.Singletons (SingInstance(..), singInstance, withSomeSing)

import AlphaBeta.GameValue (SGameValue, SomeGameValue(..))
import qualified AlphaBeta.GameValue as GameValue
import Game (Action, Game, Next(..), Position, makeMove, next)
import Strategy (Strategy(..))

type OrderingRule g = Position g -> [Action g] -> [Action g]
newtype AlphaBeta g = AlphaBeta{orderingRule :: OrderingRule g}

instance Game g => Strategy (AlphaBeta g) g where
  evaluate g AlphaBeta{orderingRule} x = GameValue.evaluate (go x)
    where
      go :: Position g -> SomeGameValue
      go pos = case next g pos of
        Options player acts -> withSomeSing player $ (. singInstance) $ \case
          (SingInstance :: SingInstance player) ->
            let
              nextValues :: [SGameValue player]
              nextValues = map
                (GameValue.setSide . go . makeMove g pos)
                (orderingRule pos acts)
            in SomeSided $ GameValue.maximum nextValues
        End v -> SomePrim v
