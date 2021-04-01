{-# LANGUAGE DefaultSignatures #-}
module Control.Category.Monoidal where

import Control.Category (Category, (>>>), (<<<))
import qualified Control.Category as C

class Category k => MonoidalProduct k where
  (***) :: (al `k` bl) -> (ar `k` br) -> ((al, ar) `k` (bl, br))

class Category k => MonoidalSum k where
  (+++) :: (al `k` bl) -> (ar `k` br) -> ((Either al ar) `k` (Either bl br))

class Category k => Symmetric k where
  swap :: (l, r) `k` (r, l)
  swapE :: (Either l r) `k` (Either r l)
  reassoc :: (a, (b, c)) `k` ((a, b), c)
  reassoc' :: (a, (b, c)) `k` ((a, b), c)
  reassocE :: (Either a (Either b c)) `k` Either (Either a b) c
  reassocE' :: Either (Either a b) c `k` (Either a (Either b c))
