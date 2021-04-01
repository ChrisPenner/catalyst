module Control.Category.Recursive where

import Control.Category (Category, (>>>), (<<<))

class Category k => Recursive k where
  recurseL :: (Either a s `k` Either b s) -> (a `k` b)
  recurseR :: (Either s a `k` Either s b) -> (a `k` b)

class Category k => Fixed k where
  fixL :: ((a, s) `k` (b, s)) -> (a `k` b)
  fixR :: ((s, a) `k` (s, b)) -> (a `k` b)
