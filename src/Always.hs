{-# LANGUAGE PolyKinds #-}

module Always
    ( Always(..)
    ) where

import Control.Monad.ST (ST)
import Data.Constraint (Constraint, Dict(Dict))

class Always (c :: k -> Constraint) f where
  dict :: forall proxy x. proxy x -> Dict (c (f x))

instance Always Monad ST where dict = const Dict
