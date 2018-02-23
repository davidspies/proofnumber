module Runnable
    ( Runnable(..)
    , runProxy
    ) where

import Control.Monad.ST (ST, runST)
import Data.Constraint (Dict(..))
import Data.Proxy (Proxy(Proxy))

import Always (Always(..))

class Always Monad m => Runnable (m :: * -> * -> *) where
  run :: (forall t. m t a) -> a

instance Runnable ST where
  run = runST

runProxy :: forall m a. Runnable m => (forall s. Monad (m s) => Proxy s -> m s a) -> a
runProxy act = run go
  where
    go :: forall s. m s a
    go = case Always.dict Proxy :: Dict (Monad (m s)) of Dict -> act Proxy
