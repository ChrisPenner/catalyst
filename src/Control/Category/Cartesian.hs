module Control.Category.Cartesian where

import Control.Category.Monoidal

import Control.Category (Category, (>>>), (<<<))

class Category k => Cartesian k where
  copy :: a `k` (a, a)
  consume :: a `k` ()
  fst' :: (l, r) `k` l
  snd' :: (l, r) `k` r

instance Cartesian (->) where
  copy x = (x, x)
  consume _ = ()
  fst' = fst
  snd' = snd

(&&&) :: (Cartesian k, MonoidalProduct k) => (a `k` l) -> (a `k` r) -> (a `k` (l, r))
(&&&) l r = copy >>> (l *** r)

class Category k => Cocartesian k where
  injectL :: a `k` (Either a b)
  injectR :: a `k` (Either b a)
  unify :: Either a a `k` a

instance Cocartesian (->) where
  injectL = Left
  injectR = Right
  unify = either id id

(|||) :: (Cocartesian k, MonoidalSum k) => (al `k` b) -> (ar `k` b) -> (Either al ar `k` b)
(|||) l r = (l +++ r) >>> unify
