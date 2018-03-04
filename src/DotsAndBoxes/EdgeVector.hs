module DotsAndBoxes.EdgeVector
    ( EdgeVector
    , empty
    , insert
    , member
    ) where

import Data.Bits (setBit, testBit)
import Data.Hashable (Hashable)
import Data.Proxy (Proxy(Proxy))
import Data.Reflection (Reifies, reflect)

import DotsAndBoxes.Internal

newtype EdgeVector gs = EdgeVector Integer
  deriving (Eq, Hashable)

empty :: EdgeVector gs
empty = EdgeVector 0

member :: Reifies gs GridSize => Edge gs -> EdgeVector gs -> Bool
member e (EdgeVector bv) = maybe False (testBit bv) (edgeToInt e)

insert :: Reifies gs GridSize => Edge gs -> EdgeVector gs -> EdgeVector gs
insert e (EdgeVector bv) = EdgeVector $ maybe bv (setBit bv) (edgeToInt e)

inBounds :: forall gs. Reifies gs GridSize => Edge gs -> Bool
inBounds Edge{row, col, dir} = case dir of
    H -> row >= 0 && row <= nrows && col > 0 && col <= ncols
    V -> row > 0 && row <= nrows && col >= 0 && col <= ncols
  where
    GridSize{nrows, ncols} = reflect (Proxy :: Proxy gs)

edgeToInt :: forall gs. Reifies gs GridSize => Edge gs -> Maybe Int
edgeToInt e@Edge{row, col, dir}
    | inBounds e = Just $ case dir of
        H -> row * ncols + col - 1
        V -> nhorizEdges + (row - 1) * (ncols + 1) + col
    | otherwise = Nothing
  where
    GridSize{nrows, ncols} = reflect (Proxy :: Proxy gs)
    nhorizEdges = (nrows + 1) * ncols + 1
