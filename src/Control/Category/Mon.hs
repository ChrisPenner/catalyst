{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Category.Mon where

import Control.Category as C
import Control.Arrow
import Data.Profunctor
import Data.Coerce (coerce)
import Data.Profunctor.Traversing

newtype Mon m a b = Mon {getMon :: m}
  deriving newtype (Semigroup, Monoid)

instance (Monoid m) => Category (Mon m) where
  id = Mon mempty
  Mon ml . Mon mr = Mon (mr <> ml)

instance (Monoid m) => Arrow (Mon m) where
  arr _ = Mon mempty
  first (Mon m) = Mon m

instance Profunctor (Mon m) where
  dimap _ _ = coerce

instance Strong (Mon m) where
  first' = coerce

instance Choice (Mon m) where
  left' = coerce

instance Traversing (Mon m) where
  wander _ = coerce

instance Closed (Mon m) where
  closed = coerce

instance Mapping (Mon m) where
