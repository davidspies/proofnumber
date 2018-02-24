module Runnable
    ( Runnable(..)
    ) where

import Control.Monad.ST (ST, runST)

class Runnable (m :: * -> * -> *) where
  run :: (forall t. Monad (m t) => m t a) -> a

instance Runnable ST where
  run = runST
