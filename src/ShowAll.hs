module ShowAll
    ( ShowAll(..)
    ) where

import Data.Constraint (Dict)
import Data.Proxy (Proxy)

class ShowAll f where
  dict :: Proxy (f x) -> Dict (Show (f x))
