{-# LANGUAGE PolyKinds #-}

module AlphaBeta
    ( AlphaBeta(..)
    , OrderingRule
    ) where

import Data.Constraint (Dict(Dict))
import Data.Function.Pointless ((.:))
import Data.Proxy (Proxy(..))
import Prelude hiding (Either(..), maximum)

import Game (Action, Game, Next(..), Position, makeMove, next)
import qualified Game.Value as GameValue
import LazyMax (LazyMax)
import qualified LazyMax
import Player (Player(..))
import Strategy (Strategy(..))

type OrderingRule g = Position g -> [Action g]
newtype AlphaBeta g = AlphaBeta{orderingRule :: OrderingRule g}

data GameValue' (player :: Player)
  = Single GameValue.GameValue
  | FlippedValue (GameValue (Opposite player))

newtype GameValue (player :: Player) = GameValue (LazyMax (GameValue' player))
  deriving (Eq, Ord)

data SomeGameValue where
  InternalGV :: IsPlayer player => GameValue player -> SomeGameValue
  LeafGV :: GameValue.GameValue -> SomeGameValue

instance IsPlayer player => Eq (GameValue' player) where
  (==) = (== EQ) .: compare

comparisonOf :: forall p b. IsPlayer p => (forall x. Ord x => x -> x -> b)
  -> GameValue' p -> GameValue' p -> b
comparisonOf f = go
  where
    go (Single x) (Single y) =
      let p = getPlayer (Proxy :: Proxy p) in
      f (GameValue.utility p x) (GameValue.utility p y)
    go (Single x) (FlippedValue y) =
      case proveOpposite (Proxy :: Proxy p) of
        Dict -> f y (GameValue $ LazyMax.pure $ Single x)
    go (FlippedValue x) (Single y) =
      case proveOpposite (Proxy :: Proxy p) of
        Dict -> f (GameValue $ LazyMax.pure $ Single y) x
    go (FlippedValue x) (FlippedValue y) =
      case proveOpposite (Proxy :: Proxy p) of
        Dict -> f y x

withProxy :: proxy s -> a s -> a s
withProxy = const id

instance IsPlayer p => Ord (GameValue' p) where
  compare = comparisonOf compare
  (<) = comparisonOf (<)
  (>) = comparisonOf (>)
  (<=) = comparisonOf (<=)
  (>=) = comparisonOf (>=)

data SingPlayer p where
  LeftSing :: SingPlayer 'Left
  RightSing :: SingPlayer 'Right

class IsPlayer (player :: Player) where
  getPlayer :: proxy player -> Player
  singPlayer :: proxy player -> SingPlayer player
  proveOpposite :: proxy player -> Dict (IsPlayer (Opposite player))
instance IsPlayer 'Left where
  getPlayer _ = Left
  singPlayer _ = LeftSing
  proveOpposite _ = Dict
instance IsPlayer 'Right where
  getPlayer _ = Right
  singPlayer _ = RightSing
  proveOpposite _ = Dict

type family Opposite (player :: Player) where
  Opposite 'Left  = 'Right
  Opposite 'Right = 'Left

evaluate' :: IsPlayer p => GameValue p -> GameValue.GameValue
evaluate' gv@(GameValue x) = case LazyMax.get x of
  Single y       -> y
  FlippedValue y -> case proveOpposite gv of Dict -> evaluate' y

reifyPlayer :: Player -> (forall p. IsPlayer p => Proxy p -> a) -> a
reifyPlayer player f = case player of
  Player.Left  -> f (Proxy :: Proxy 'Player.Left)
  Player.Right -> f (Proxy :: Proxy 'Player.Right)

setSide :: (IsPlayer p1, IsPlayer p2)
  => Proxy p1 -> GameValue p2 -> GameValue p1
setSide proxy gv = case (singPlayer proxy, singPlayer gv) of
  (LeftSing, LeftSing)   -> gv
  (RightSing, RightSing) -> gv
  (LeftSing, RightSing)  -> GameValue $ LazyMax.pure $ FlippedValue gv
  (RightSing, LeftSing)  -> GameValue $ LazyMax.pure $ FlippedValue gv

maximum :: IsPlayer p => [GameValue p] -> GameValue p
maximum = GameValue . LazyMax.maximum . map (\(GameValue gv) -> gv)

instance Game g => Strategy (AlphaBeta g) g where
  evaluate g AlphaBeta{orderingRule} x = case go x of
      InternalGV v -> evaluate' v
      LeafGV v     -> v
    where
      go :: Position g -> SomeGameValue
      go pos = case next g pos of
        Options player _ -> reifyPlayer player $ \(proxy :: Proxy p) ->
          let
            acts = orderingRule pos
            nextPos = map (makeMove g pos) acts
            f :: SomeGameValue -> GameValue p
            f = \case
              LeafGV gv     -> GameValue $ LazyMax.pure $ Single gv
              InternalGV gv -> setSide proxy gv
          in InternalGV $ withProxy proxy $ maximum $ map (f . go) nextPos
        End v -> LeafGV v
