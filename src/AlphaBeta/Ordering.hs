module AlphaBeta.Ordering
    ( random
    ) where

import DSpies.Prelude

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Hashable (Hashable, hash)
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import Data.Word (Word64)
import qualified System.Random.PCG as Rand

shuffle :: Rand.GenST s -> [a] -> ST s [a]
shuffle g xs = do
  vec <- Vec.thaw $ Vec.fromList xs
  let vlen = MVec.length vec
  forM_ [0..(vlen - 1)] $ \i -> do
    j <- Rand.uniformR (i, vlen - 1) g
    MVec.swap vec i j
  Vec.toList <$> Vec.freeze vec

random :: Hashable src => Word64 -> src -> [a] -> [a]
random seed pos acts = runST $ do
  r <- Rand.initialize seed (fromIntegral $ hash pos)
  shuffle r acts
