{-# LANGUAGE DeriveAnyClass #-}
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

import DSpies.Prelude hiding (Either(..))

import Data.Hashable (Hashable)
import Data.Singletons.TH

$(singletons [d|
  data Player = Left | Right
    deriving (Eq, Show, Generic, Hashable)
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
