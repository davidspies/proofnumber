module AlphaBeta
    ( AlphaBeta(..)
    , OrderingRule
    ) where

import DSpies.Prelude

import Data.Hashable (Hashable(..))
import Data.HashTable.Memo (memo)
import Data.Singletons (SingInstance(..), singInstance, withSomeSing)

import AlphaBeta.GameValue (SGameValue, SomeGameValue(..))
import qualified AlphaBeta.GameValue as GameValue
import Game (Action, Game, Next(..), Position, makeMove, next)
import Strategy (Strategy(..))

type OrderingRule g = Position g -> [Action g] -> [Action g]
newtype AlphaBeta g = AlphaBeta{orderingRule :: OrderingRule g}

instance (Eq (Position g), Hashable (Position g), Game g)
    => Strategy (AlphaBeta g) g where
  evaluate g AlphaBeta{orderingRule} = GameValue.evaluate . go
    where
      makeMove' = makeMove g
      next' = next g
      go :: Position g -> SomeGameValue
      go = memo $ \pos -> case next' pos of
        Options player acts -> withSomeSing player $ (. singInstance) $ \case
          (SingInstance :: SingInstance player) ->
            let
              nextValues :: [SGameValue player]
              nextValues = map
                (GameValue.setSide . go . makeMove' pos)
                (orderingRule pos acts)
            in SomeSided $ GameValue.maximum nextValues
        End v -> SomePrim v
