{-# LANGUAGE DefaultSignatures #-}

module AlphaBeta.GameValue
    ( SomeGameValue(..)
    , SGameValue
    , evaluate
    , maximum
    , setSide
    ) where

import DSpies.Prelude hiding (maximum)

import Data.Coerce (coerce)
import Data.Singletons (SingI, SingInstance(..), fromSing, sing)

import qualified Game.Value as Primitive
import LazyMax (LazyMax)
import qualified LazyMax
import Player (Opposite, Player(..), SPlayer, Sing(..), oppSingInstance)

class ToGameValue gv where
  gvHelper :: (forall gv2. IsGameValue gv2 => gv2 -> out) -> gv -> out
class IsGameValue gv where
  evaluate :: gv -> Primitive.GameValue
  default evaluate :: ToGameValue gv => gv -> Primitive.GameValue
  evaluate = gvHelper evaluate
  setSide :: SingI player => gv -> SGameValue player
  default setSide :: (SingI player, ToGameValue gv) => gv -> SGameValue player
  setSide = gvHelper setSide

instance IsGameValue Primitive.GameValue where
  evaluate = id
  setSide = fromGV . Prim

data GV (player :: Player)
  = Prim Primitive.GameValue
  | Flipped (SGameValue (Opposite player))
instance SingI player => ToGameValue (GV player) where
  gvHelper f = \case
    Prim gv -> f gv
    Flipped gv -> case oppSingInstance (sing :: Sing player) of
      SingInstance -> f gv
instance SingI player => IsGameValue (GV player)

comparisonOf :: forall out player. SingI player
  => (forall x. Ord x => x -> x -> out) -> GV player -> GV player -> out
comparisonOf f = curry go
  where
    splayer :: SPlayer player
    splayer = sing
    playerUtil = Primitive.utility (fromSing splayer)
    oppSingInst = oppSingInstance splayer
    go = \case
      (Prim x, Prim y) -> f (playerUtil x) (playerUtil y)
      (Prim x, Flipped y) -> case oppSingInst of
        SingInstance -> f y (setSide x)
      (Flipped x, Prim y) -> case oppSingInst of
        SingInstance -> f (setSide y) x
      (Flipped x, Flipped y) -> case oppSingInst of
        SingInstance -> f y x
instance SingI player => Eq (GV player) where
  (==) = (== EQ) .: compare
instance SingI player => Ord (GV player) where
  compare = comparisonOf compare
  (<)  = comparisonOf (<)
  (>)  = comparisonOf (>)
  (<=) = comparisonOf (<=)
  (>=) = comparisonOf (>=)

newtype SGameValue (player :: Player) =
    SGameValue {unSGameValue :: LazyMax (GV player)}
  deriving (Eq, Ord)
instance SingI player => IsGameValue (SGameValue player) where
  evaluate = evaluate . LazyMax.get . unSGameValue
  setSide :: forall pout. SingI pout => SGameValue player -> SGameValue pout
  setSide = case (sing :: SPlayer player) of
    SLeft -> case (sing :: SPlayer pout) of
      SLeft  -> id
      SRight -> fromGV . Flipped
    SRight -> case (sing :: SPlayer pout) of
      SLeft  -> fromGV . Flipped
      SRight -> id

fromGV :: GV player -> SGameValue player
fromGV = SGameValue . LazyMax.pure

maximum :: forall p. SingI p => [SGameValue p] -> SGameValue p
maximum = coerce (LazyMax.maximum :: [LazyMax (GV p)] -> LazyMax (GV p))

data SomeGameValue where
  SomePrim  :: Primitive.GameValue -> SomeGameValue
  SomeSided :: SingI player => SGameValue player -> SomeGameValue
instance ToGameValue SomeGameValue where
  gvHelper f = \case {SomePrim v -> f v; SomeSided v -> f v}
instance IsGameValue SomeGameValue
