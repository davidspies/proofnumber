module Game.Value.Internal
    ( GameValue(..)
    ) where

import DSpies.Prelude

newtype GameValue = GameValue {leftUtility :: Double}
  deriving (Eq, Show)
