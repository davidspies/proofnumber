module Player
    ( Player(..)
    , PlayerMap
    , (!)
    , fromList
    , insert
    , opposite
    ) where

import Data.List (foldl')
import Prelude hiding (Either(..))

data Player = Left | Right
  deriving (Show)
data PlayerMap a = PlayerMap a a

(!) :: PlayerMap a -> Player -> a
(!) (PlayerMap l r) = \case
  Left -> l
  Right -> r

insert :: Player -> a -> PlayerMap a -> PlayerMap a
insert Left x (PlayerMap _ r)  = PlayerMap x r
insert Right x (PlayerMap l _) = PlayerMap l x

opposite :: Player -> Player
opposite = \case
  Left -> Right
  Right -> Left

fromList :: [(Player, a)] -> PlayerMap a
fromList =
  foldl'
    (flip $ uncurry insert)
    (PlayerMap (error "uninitialized") (error "uninitialized"))
