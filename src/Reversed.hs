module Reversed
    ( Reversed
    , empty
    , init
    , null
    , snoc
    , toList
    , tryUnsnoc
    ) where

import DSpies.Prelude hiding (empty, init, null, toList)
import qualified DSpies.Prelude as P

newtype Reversed a = Reversed [a]

empty :: Reversed a
empty = Reversed []

snoc :: Reversed a -> a -> Reversed a
snoc (Reversed xs) x = Reversed (x : xs)

toList :: Reversed a -> [a]
toList (Reversed xs) = reverse xs

init :: HasCallStack => Reversed a -> Reversed a
init (Reversed xs) = Reversed $ tail xs

null :: Reversed a -> Bool
null (Reversed xs) = P.null xs

tryUnsnoc :: Reversed a -> Maybe (Reversed a, a)
tryUnsnoc (Reversed xs) = case xs of
  []       -> Nothing
  (y : ys) -> Just (Reversed ys, y)
