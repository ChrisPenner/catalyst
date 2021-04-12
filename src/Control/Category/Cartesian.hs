module Control.Category.Cartesian where

import Control.Category.Monoidal

import Control.Category ((>>>))

class MonoidalProduct k => Cartesian k where
  (&&&) :: (a `k` l) -> (a `k` r) -> (a `k` (l, r))
  l &&& r = copy >>> (l *** r)
  consume :: a `k` ()
  copy :: a `k` (a, a)
  fst' :: (l, r) `k` l
  snd' :: (l, r) `k` r

instance Cartesian (->) where
  copy x = (x, x)
  consume _ = ()
  fst' = fst
  snd' = snd

class MonoidalSum k => Cocartesian k where
  (|||) :: (al `k` b) -> (ar `k` b) -> (Either al ar `k` b)
  (|||) l r = (l +++ r) >>> unify

  injectL :: a `k` (Either a b)
  injectR :: a `k` (Either b a)
  unify :: Either a a `k` a
  -- | tags 'Right' when 'True', 'Left' when 'False'
  tag :: k (Bool, a) (Either a a)

instance Cocartesian (->) where
  injectL = Left
  injectR = Right
  unify = either id id
  tag (True, a) = Right a
  tag (False, a) = Left a
