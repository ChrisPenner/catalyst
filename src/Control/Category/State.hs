{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Category.State where
import qualified Control.Category as C
import Control.Arrow

newtype State s a b = State ((s, a) -> (s, b))

instance C.Category (State s) where
  id = State C.id
  State l . State r = State (l C.. r)


class (C.Category p) => CategoryState s p | p -> s where
  {-# MINIMAL (get, put) #-}
  get :: p a s
  put :: p s ()
  state :: p s s -> p s ()
  state p = (get >>> p >>> put)


instance CategoryState s (State s) where
  get = State (fst &&& fst)
  put = State (\(_, newS) -> (newS, ()))

-- modify :: State s (s -> s) s
-- modify = State (\(s, f) -> let newS = f s in (newS, newS))
