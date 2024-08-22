{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.CoProd where
import qualified Control.Category as C
import Data.Profunctor
import Control.Category.Free as CF
import qualified Control.Category.Free as CF
import qualified Control.Category.Free.Internal as CF
import Control.Category.State

data VoidC a b

absurdC :: VoidC a b -> x
absurdC x = case x of {}

data CoProd c d a b =
      L (c a b)
    | R (d a b)

unCompose :: (c a b -> k x y) -> (d a b -> k x y) -> CoProd c d a b -> k x y
unCompose l _ (L c) = l c
unCompose _ r (R d) = r d

infixr 9 `CatCompose`
newtype CatCompose c d a b = CatCompose (CF.ListTr (CoProd c d) a b)

class CategoryTrans ct where
  liftR :: c a b -> ct k c a b
  liftL :: c a b -> ct c k a b

instance CategoryTrans CatCompose where
  liftR c = CatCompose (CF.liftL (R c))
  liftL c = CatCompose (CF.liftL (L c))

instance (C.Category c, C.Category d, Profunctor c, Profunctor d) => C.Category (CatCompose c d) where
  id = CatCompose C.id
  CatCompose l . CatCompose r = CatCompose (l C.. r)

interp :: forall c d k a b. C.Category k => (forall x y. c x y -> k x y) -> (forall x y. d x y -> k x y) -> CF.ListTr (CoProd c d) a b -> k a b
interp l r f = CF.foldlL go C.id f
  where
    go :: k y z -> CoProd c d x y -> k x z
    go k comp = unCompose l r comp C.>>> k

example :: (x `CatCompose` k `CatCompose` (State s)) a s
example = liftR . liftR $ get
