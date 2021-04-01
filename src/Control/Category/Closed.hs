module Control.Category.Closed where

import Control.Category (Category, (>>>), (<<<))

class Category k => Closed k where
  -- closed :: (a `k` b) -> ((r -> a) `k` (r -> b))
  apply :: ((a -> b), a) `k` b
  curry' :: ((a, b) `k` c) -> (a `k` (b -> c))
  uncurry' :: (a `k` (b -> c)) -> ((a, b) `k` c)
