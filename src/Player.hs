{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Player
    ( Opposite
    , Player(..)
    , Sing(..)
    , SPlayer
    , opposite
    , oppSingInstance
    ) where

import Data.Singletons.TH
import Prelude hiding (Either(..))

$(singletons [d|
  data Player = Left | Right
    deriving (Show)
  |])

$(promote [d|
  opposite :: Player -> Player
  opposite p = case p of
    Left  -> Right
    Right -> Left
  |])

oppSingInstance :: SPlayer p -> SingInstance (Opposite p)
oppSingInstance = \case
  SLeft  -> SingInstance
  SRight -> SingInstance
