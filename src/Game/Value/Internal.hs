module Game.Value.Internal
    ( GameValue(..)
    ) where

newtype GameValue = GameValue {leftUtility :: Double}
  deriving (Eq, Show)
