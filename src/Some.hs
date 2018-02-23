{-# LANGUAGE UndecidableInstances #-}

module Some where

import Data.Constraint (Dict(..))
import Data.Functor.Classes (showsUnaryWith)

import Always (Always)
import qualified Always

data Some a = forall x. Some (a x)

instance Always Show a => Show (Some a) where
  showsPrec d (Some (x :: a x)) = case Always.dict x :: Dict (Show (a x)) of
    Dict -> showsUnaryWith showsPrec "Some" d x
