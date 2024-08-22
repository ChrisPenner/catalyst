module Control.Category.Product where

import Control.Category as C
import Control.Arrow
import Data.Profunctor
import Data.Profunctor.Traversing

newtype CatProd c1 c2 a b =
    CatProd (c1 a b, c2 a b)

instance (Category c1, Category c2) => Category (CatProd c1 c2) where
  id = CatProd (C.id, C.id)
  CatProd (l1, l2) . CatProd (r1, r2) = CatProd (l1 C.. r1, l2 C.. r2)

instance (Arrow c1, Arrow c2) => Arrow (CatProd c1 c2) where
  arr f = CatProd (arr f, arr f)
  first (CatProd (l, r)) = CatProd (first l, first r)

instance (Profunctor c1, Profunctor c2) => Profunctor (CatProd c1 c2) where
  dimap l r (CatProd (c1, c2)) = CatProd (dimap l r c1, dimap l r c2)

instance (Strong c1, Strong c2) => Strong (CatProd c1 c2) where
  first' (CatProd (l, r)) = CatProd (first' l, first' r)

instance (Choice c1, Choice c2) => Choice (CatProd c1 c2) where
  left' (CatProd (l, r)) = CatProd (left' l, left' r)

instance (Traversing c1, Traversing c2) => Traversing (CatProd c1 c2) where
  wander f (CatProd (l, r)) = CatProd (wander f l, wander f r)

instance (Closed c1, Closed c2) => Closed (CatProd c1 c2) where
  closed (CatProd (l, r)) = CatProd (closed l, closed r)

instance (Mapping c1, Mapping c2) => Mapping (CatProd c1 c2) where
