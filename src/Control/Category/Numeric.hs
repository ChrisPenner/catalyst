module Control.Category.Numeric where


class Numeric k where
  num :: Int -> k a Int
  add :: k (Int, Int) Int
  negate' :: k Int Int
  mult :: k (Int, Int) Int
  div' :: k (Int, Int) Int
  mod' :: k (Int, Int) Int

instance Numeric (->) where
  num = const
  add = uncurry (+)
  negate' = negate
  mult = uncurry (*)
  div' = uncurry div
  mod' = uncurry mod

