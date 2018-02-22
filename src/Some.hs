module Some where

import Data.Constraint (Dict(..))
import Data.Functor.Classes (showsUnaryWith)
import Data.Proxy (Proxy(Proxy))

import ShowAll (ShowAll)
import qualified ShowAll

data Some a = forall x. Some (a x)

proxy :: a -> Proxy a
proxy = const Proxy

instance ShowAll a => Show (Some a) where
  showsPrec d (Some x) = case ShowAll.dict (proxy x) of
    Dict -> showsUnaryWith showsPrec "Some" d x
