module AlphaBeta.Ordering
    ( random
    ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import qualified System.Random.PCG as Rand

import AlphaBeta

shuffle :: Rand.GenST s -> [a] -> ST s [a]
shuffle g xs = do
  vec <- Vec.thaw $ Vec.fromList xs
  let vlen = MVec.length vec
  forM_ [0..(vlen - 1)] $ \i -> do
    j <- Rand.uniformR (i, vlen - 1) g
    MVec.swap vec i j
  Vec.toList <$> Vec.freeze vec

random :: forall g s. Rand.FrozenGen -> ST s (OrderingRule g (ST s))
random seed = do
  r <- Rand.restore seed
  return $ OrderingRule (shuffle r)
