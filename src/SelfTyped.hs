module SelfTyped
    ( SelfTyped
    , SomeSelfTyped(..)
    , get
    , selfTyped
    , withProxy
    ) where

import Data.Reflection (Reifies, reify)

newtype SelfTyped a s = SelfTyped a
data SomeSelfTyped a = forall s. Reifies s a => SomeSelfTyped (SelfTyped a s)

selfTyped :: a -> SomeSelfTyped a
selfTyped x = reify x (SomeSelfTyped . withProxy (SelfTyped x))

withProxy :: a s -> proxy s -> a s
withProxy = const

get :: SelfTyped a s -> a
get (SelfTyped x) = x
