module Control.Category.Recursive where

import Control.Category (Category, (>>>), (<<<))
import qualified Data.Profunctor.Choice as P
import qualified Data.Profunctor.Strong as P

class Category k => Recursive k where
  recurseL :: (Either a s `k` Either b s) -> (a `k` b)
  recurseR :: (Either s a `k` Either s b) -> (a `k` b)

instance Recursive (->) where
  recurseL = P.unleft
  recurseR = P.unright

class Category k => Fixed k where
  fixL :: ((a, s) `k` (b, s)) -> (a `k` b)
  fixR :: ((s, a) `k` (s, b)) -> (a `k` b)

instance Fixed (->) where
  fixL = P.unfirst
  fixR = P.unsecond
