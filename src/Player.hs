{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Player
    ( Opposite
    , Player(..)
    , PlayerMap
    , Sing(..)
    , SPlayer
    , (!)
    , fromList
    , insert
    , opposite
    , proveSingIOpposite
    ) where

import Data.Constraint (Dict(Dict))
import Data.List (foldl')
import Data.Singletons.TH
import Prelude hiding (Either(..))

$(singletons [d|
  data Player = Left | Right
    deriving (Show)
  |])

$(promote [d|
  opposite :: Player -> Player
  opposite p = case p of
    Left  -> Right
    Right -> Left
  |])

proveSingIOpposite :: SPlayer p -> Dict (SingI (Opposite p))
proveSingIOpposite = \case
  SLeft -> Dict
  SRight -> Dict

data PlayerMap a = PlayerMap a a

(!) :: PlayerMap a -> Player -> a
(!) (PlayerMap l r) = \case
  Left -> l
  Right -> r

insert :: Player -> a -> PlayerMap a -> PlayerMap a
insert Left x (PlayerMap _ r)  = PlayerMap x r
insert Right x (PlayerMap l _) = PlayerMap l x

fromList :: [(Player, a)] -> PlayerMap a
fromList =
  foldl'
    (flip $ uncurry insert)
    (PlayerMap (error "uninitialized") (error "uninitialized"))
