module AlphaBeta
    ( AlphaBeta(..)
    , OrderingRule
    ) where

import Data.Constraint (Dict(Dict))
import Data.Function.Pointless ((.:))
import Data.Singletons (SingI, fromSing, sing, withSingI, withSomeSing)
import Prelude hiding (Either(..), maximum)

import Game (Action, Game, Next(..), Position, makeMove, next)
import qualified Game.Value as GameValue
import LazyMax (LazyMax)
import qualified LazyMax
import Player (Opposite, Player(..), SPlayer, Sing(..), proveSingIOpposite)
import Strategy (Strategy(..))

type OrderingRule g = Position g -> [Action g]
newtype AlphaBeta g = AlphaBeta{orderingRule :: OrderingRule g}

data GameValue' (player :: Player)
  = Single GameValue.GameValue
  | FlippedValue (GameValue (Opposite player))

newtype GameValue (player :: Player) = GameValue (LazyMax (GameValue' player))
  deriving (Eq, Ord)

data SomeGameValue where
  PGV :: SingI player => GameValue player -> SomeGameValue
  GV :: GameValue.GameValue -> SomeGameValue

instance SingI player => Eq (GameValue' player) where
  (==) = (== EQ) .: compare

comparisonOf :: forall p b. SingI p => (forall x. Ord x => x -> x -> b)
  -> GameValue' p -> GameValue' p -> b
comparisonOf f = go
  where
    singPlayer :: SPlayer p
    singPlayer = sing
    go (Single x) (Single y) =
      let p = fromSing singPlayer in
      f (GameValue.utility p x) (GameValue.utility p y)
    go (Single x) (FlippedValue y) = case proveSingIOpposite singPlayer of
      Dict -> f y (GameValue $ LazyMax.pure $ Single x)
    go (FlippedValue x) (Single y) = case proveSingIOpposite singPlayer of
      Dict -> f (GameValue $ LazyMax.pure $ Single y) x
    go (FlippedValue x) (FlippedValue y) = case proveSingIOpposite singPlayer of
      Dict -> f y x

instance SingI p => Ord (GameValue' p) where
  compare = comparisonOf compare
  (<)  = comparisonOf (<)
  (>)  = comparisonOf (>)
  (<=) = comparisonOf (<=)
  (>=) = comparisonOf (>=)

evaluate' :: forall p. SingI p => GameValue p -> GameValue.GameValue
evaluate' (GameValue x) = case LazyMax.get x of
  Single y       -> y
  FlippedValue y -> case proveSingIOpposite (sing :: SPlayer p) of
    Dict -> evaluate' y

setSide :: forall p1 p2. SingI p2 => SPlayer p1 -> GameValue p2 -> GameValue p1
setSide l gv = case (l, sing :: SPlayer p2) of
  (SLeft, SLeft)   -> gv
  (SRight, SRight) -> gv
  (SLeft, SRight)  -> GameValue $ LazyMax.pure $ FlippedValue gv
  (SRight, SLeft)  -> GameValue $ LazyMax.pure $ FlippedValue gv

maximum :: SingI p => [GameValue p] -> GameValue p
maximum = GameValue . LazyMax.maximum . map (\(GameValue gv) -> gv)

instance Game g => Strategy (AlphaBeta g) g where
  evaluate g AlphaBeta{orderingRule} x = case go x of
      PGV v -> evaluate' v
      GV v  -> v
    where
      go :: Position g -> SomeGameValue
      go pos = case next g pos of
        Options player _ -> withSomeSing player $ \(splayer :: SPlayer p) ->
          let
            acts = orderingRule pos
            nextPos = map (makeMove g pos) acts
            f :: SomeGameValue -> GameValue p
            f = \case
              GV gv  -> GameValue $ LazyMax.pure $ Single gv
              PGV gv -> setSide splayer gv
          in withSingI splayer $ PGV $ maximum $ map (f . go) nextPos
        End v -> GV v
