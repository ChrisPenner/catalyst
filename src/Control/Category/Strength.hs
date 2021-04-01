module Control.Category.Strength where

import Control.Category (Category, (>>>), (<<<))
import qualified Control.Category as C

class Category k => Strong k where
  first' :: (a `k` b) -> ((a, c) `k` (b, c))
  second' :: (a `k` b) -> ((c, a) `k` (c, b))

class Category k => Choice k where
  left :: (a `k` b) -> ((Either a c) `k` (Either b c))
  right :: (a `k` b) -> ((Either c a) `k` (Either c b))
