{-# LANGUAGE DefaultSignatures #-}
module Control.Category.Monoidal where

import Control.Category (Category, (>>>))
import qualified Control.Arrow as A

class SymmetricProduct k => MonoidalProduct k where
  {-# MINIMAL first' | second' #-}
  (***) :: (al `k` bl) -> (ar `k` br) -> ((al, ar) `k` (bl, br))
  l *** r = first' l >>> second' r
  first' :: (a `k` b) -> ((a, c) `k` (b, c))
  first' f = swap >>> second' f >>> swap
  second' :: (a `k` b) -> ((c, a) `k` (c, b))
  second' f = swap >>> first' f >>> swap

instance MonoidalProduct (->) where
  (***) = (A.***)
  first' = A.first
  second' = A.second

class SymmetricSum k => MonoidalSum k where
  {-# MINIMAL left | right #-}
  (+++) :: (al `k` bl) -> (ar `k` br) -> ((Either al ar) `k` (Either bl br))
  l +++ r = left l >>> right r
  left :: (a `k` b) -> ((Either a c) `k` (Either b c))
  left f = swapE >>> right f >>> swapE
  right :: (a `k` b) -> ((Either c a) `k` (Either c b))
  right f = swapE >>> left f >>> swapE

instance MonoidalSum (->) where
  l +++ r = l A.+++ r
  left = A.left
  right = A.right

class Category k => SymmetricProduct k where
  swap :: (l, r) `k` (r, l)
  reassoc :: (a, (b, c)) `k` ((a, b), c)

class Category k => SymmetricSum k where
  swapE :: (Either l r) `k` (Either r l)
  reassocE :: (Either a (Either b c)) `k` Either (Either a b) c

instance SymmetricProduct (->) where
  swap (a, b) = (b, a)
  reassoc (a, (b, c)) = ((a, b), c)

instance SymmetricSum (->) where
  swapE (Left a) = Right a
  swapE (Right a) = Left a
  reassocE (Left a) = Left (Left a)
  reassocE (Right (Left b)) = Left (Right b)
  reassocE (Right (Right c)) = Right c

class CategoryPlus k => CategoryZero k where
  zeroC :: k a b

class Category k => CategoryPlus k where
  (<+>) :: k a b -> k a b -> k a b
