{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Category.Free where

import qualified Control.Category as C
import Control.Category.Monoidal as C
import Control.Category.Cartesian as C
import Control.Category.Recursive as C
import Data.Kind ( Constraint )

-- Data kinds representing whether each constraint is required.
-- We could use 'Bool', but using separate Data Kinds helps a lot with producing nicer type errors.
data IsCategory = HasCategory | NoCategory
data IsSymmetricProduct = HasSymmetricProduct | NoSymmetricProduct
data IsSymmetricSum = HasSymmetricSum | NoSymmetricSum
data IsMonoidalProduct = HasMonoidalProduct | NoMonoidalProduct
data IsMonoidalSum = HasMonoidalSum | NoMonoidalSum
data IsCartesian = HasCartesian | NoCartesian
data IsCocartesian = HasCocartesian | NoCocartesian
data IsRecursive = HasRecursive | NoRecursive
data IsFixed = HasFixed | NoFixed

data Requirements =
    Req
        IsCategory
        IsSymmetricProduct
        IsSymmetricSum
        IsMonoidalProduct
        IsMonoidalSum
        IsCartesian
        IsCocartesian
        IsRecursive
        IsFixed

type family ConstraintsOf (x :: anykind) (k :: * -> * -> *) = (c :: Constraint) where
  -- The constraints of a 'Requirements' are the constriants of each class-requirement
  ConstraintsOf ('Req cat symP symS monP monS cart cocart rec fix) k =
      ( ConstraintsOf cat k
      , ConstraintsOf symP k
      , ConstraintsOf symS k
      , ConstraintsOf monP k
      , ConstraintsOf monS k
      , ConstraintsOf cart k
      , ConstraintsOf cocart k
      , ConstraintsOf rec k
      , ConstraintsOf fix k
      )
  -- Each sub-requirement has an associated class
  ConstraintsOf 'HasCategory cat = C.Category cat
  ConstraintsOf 'HasSymmetricProduct cat = C.SymmetricProduct cat
  ConstraintsOf 'HasSymmetricSum cat = C.SymmetricSum cat
  ConstraintsOf 'HasMonoidalProduct cat = C.MonoidalProduct cat
  ConstraintsOf 'HasMonoidalSum cat = C.MonoidalSum cat
  ConstraintsOf 'HasCartesian cat = C.Cartesian cat
  ConstraintsOf 'HasCocartesian cat = C.Cocartesian cat
  ConstraintsOf 'HasRecursive cat = C.Recursive cat
  ConstraintsOf 'HasFixed cat = C.Fixed cat

  ConstraintsOf 'NoCategory cat = ()
  ConstraintsOf 'NoSymmetricProduct cat = ()
  ConstraintsOf 'NoSymmetricSum cat = ()
  ConstraintsOf 'NoMonoidalProduct cat = ()
  ConstraintsOf 'NoMonoidalSum cat = ()
  ConstraintsOf 'NoCartesian cat = ()
  ConstraintsOf 'NoCocartesian cat = ()
  ConstraintsOf 'NoRecursive cat = ()
  ConstraintsOf 'NoFixed cat = ()

type FreeFunction c =
    Catalyst
      ('Req
          'HasCategory
          'HasSymmetricProduct
          'HasSymmetricSum
          'HasMonoidalProduct
          'HasMonoidalSum
          'HasCartesian
          'HasCocartesian
          'HasRecursive
          'HasFixed
      ) c

data Catalyst (r :: Requirements) (p :: * -> * -> *) a b where
  ID :: (r ~ 'Req 'HasCategory symP symS monP monS cart cocart rec fix) => Catalyst r p x x
  Comp :: Catalyst ('Req 'HasCategory symP symS monP monS cart cocart rec fix) p x y -> Catalyst ('Req 'HasCategory symP symS monP monS cart cocart rec fix) p y z -> Catalyst ('Req 'HasCategory symP symS monP monS cart cocart rec fix) p x z

  Swap :: (r ~ 'Req 'HasCategory 'HasSymmetricProduct symS monP monS cart cocart rec fix) => Catalyst r p (a, b) (b, a)
  Reassoc :: (r ~ 'Req 'HasCategory 'HasSymmetricProduct symS monP monS cart cocart rec fix) => Catalyst r p (a, (b, c)) ((a, b), c)

  SwapE :: (r ~ 'Req cat symP 'HasSymmetricSum monP monS cart cocart rec fix) => Catalyst r p (Either a b) (Either b a)
  ReassocE :: (r ~ 'Req cat symP 'HasSymmetricSum monP monS cart cocart rec fix) => Catalyst r p (Either a (Either b c)) (Either (Either a b) c)

  First :: (r ~ 'Req cat symP symS 'HasMonoidalProduct monS cart cocart rec fix) => Catalyst r p a b -> Catalyst r p (a, m) (b, m)
  Second :: (r ~ 'Req cat symP symS 'HasMonoidalProduct monS cart cocart rec fix) => Catalyst r p a b -> Catalyst r p (m, a) (m, b)
  -- (***)
  Alongside :: (r ~ 'Req cat symP symS 'HasMonoidalProduct monS cart cocart rec fix) => Catalyst r p a b -> Catalyst r p a' b' -> Catalyst r p (a, a') (b, b')
  -- (&&&)
  Fanout :: (r ~ 'Req cat symP symS monP monS 'HasCartesian cocart rec fix) => Catalyst r p a b -> Catalyst r p a b' -> Catalyst r p a (b, b')

  Left' :: (r ~ 'Req cat symP symS monP 'HasMonoidalSum cart cocart rec fix) => Catalyst r p a b -> Catalyst r p (Either a x) (Either b x)
  Right' :: (r ~ 'Req cat symP symS monP 'HasMonoidalSum cart cocart rec fix) => Catalyst r p a b -> Catalyst r p (Either x a) (Either x b)
  -- (+++)
  EitherOf :: (r ~ 'Req cat symP symS monP 'HasMonoidalSum cart cocart rec fix) => Catalyst r p a b -> Catalyst r p a' b' -> Catalyst r p (Either a a') (Either b b')
  -- (|||)
  Fanin :: (r ~ 'Req cat symP symS monP monS cart 'HasCocartesian rec fix) => Catalyst r p a b -> Catalyst r p a' b -> Catalyst r p (Either a a') b

  Copy :: (r ~ 'Req cat symP symS monP monS 'HasCartesian cocart rec fix) => Catalyst r p x (x, x)
  Consume :: (r ~ 'Req cat symP symS monP monS 'HasCartesian cocart rec fix) => Catalyst r p x ()
  Fst :: (r ~ 'Req cat symP symS monP monS 'HasCartesian cocart rec fix) => Catalyst r p (a, b) a
  Snd :: (r ~ 'Req cat symP symS monP monS 'HasCartesian cocart rec fix) => Catalyst r p (a, b) b

  InjectL :: (r ~ 'Req cat symP symS monP monS cart 'HasCocartesian rec fix) => Catalyst r p a (Either a b)
  InjectR :: (r ~ 'Req cat symP symS monP monS cart 'HasCocartesian rec fix) => Catalyst r p b (Either a b)
  Unify :: (r ~ 'Req cat symP symS monP monS cart 'HasCocartesian rec fix) => Catalyst r p (Either a a) a
  Tag :: (r ~ 'Req cat symP symS monP monS cart 'HasCocartesian rec fix) => Catalyst r p (Bool, a) (Either a a)

  RecurseL :: (r ~ 'Req cat symP symS monP monS cart cocart 'HasRecursive fix) => Catalyst r p (Either a d) (Either b d) -> Catalyst r p a b
  RecurseR :: (r ~ 'Req cat symP symS monP monS cart cocart 'HasRecursive fix) => Catalyst r p (Either d a) (Either d b) -> Catalyst r p a b

  FixL :: (r ~ 'Req cat symP symS monP monS cart cocart rec 'HasFixed) => Catalyst r p (a, d) (b, d) -> Catalyst r p a b
  FixR :: (r ~ 'Req cat symP symS monP monS cart cocart rec 'HasFixed) => Catalyst r p (d, a) (d, b) -> Catalyst r p a b

  LiftC :: p a b -> Catalyst r p a b

instance (forall x y. Show (c x y)) => Show (Catalyst r c a b) where
  show
    = \case
        Fst -> "fst"
        Snd -> "snd"
        Copy -> "copy"
        Consume -> "consume"
        Swap -> "swap"
        Reassoc -> "reassoc"
        SwapE -> "swapE"
        ReassocE -> "reassocE"

        InjectL -> "injectL"
        InjectR -> "injectR"
        Unify -> "unify"
        Tag -> "tag"
        (First l) -> "(first' " <> show l <> ")"
        (Second l) -> "(second' " <> show l <> ")"
        (Alongside l r) -> "(" <> show l <> " *** " <> show r <>  ")"
        (Fanout l r) -> "(" <> show l <> " &&& " <> show r  <> ")"
        (Left' l) -> "(left " <> show l <> ")"
        (Right' r) -> "(right " <> show r <> ")"
        (EitherOf l r) -> "(" <> show l <> " +++ " <> show r <> ")"
        (Fanin l r) -> "(" <> show l <> " +++ " <> show r <> ")"
        (LiftC cab) -> show cab
        ID -> "id"
        (Comp l r) -> "(" <> show l <> " >>> " <> show r <> ")"
        (RecurseL l) -> "(recurseR " <> show l <> ")"
        (RecurseR r) -> "(recurseL " <> show r <> ")"
        (FixL l) -> "(fixL " <> show l <> ")"
        (FixR r) -> "(fixR " <> show r <> ")"

runFree :: forall r p c a b. (ConstraintsOf r p) => (forall x y. c x y -> p x y) -> Catalyst r c a b -> p a b
runFree interp = \case
  LiftC c -> interp c
  Fst -> fst'
  Snd -> snd'
  Copy -> copy
  Consume -> consume
  Swap -> swap
  SwapE -> swapE
  Reassoc -> reassoc
  ReassocE -> reassocE
  InjectL -> injectL
  InjectR -> injectR
  Unify -> unify
  Tag -> tag
  First p -> first' (runFree interp p)
  Second p -> second' (runFree interp p)
  Alongside l r -> runFree interp l C.*** runFree interp r
  Fanout l r -> runFree interp l C.&&& runFree interp r
  Left' p -> left (runFree interp p)
  Right' p -> right (runFree interp p)
  Fanin l r -> runFree interp l C.||| runFree interp r
  EitherOf l r -> runFree interp l C.+++ runFree interp r
  Comp l r -> runFree interp l C.>>> runFree interp r
  ID -> C.id
  RecurseL l -> recurseL (runFree interp l)
  RecurseR r -> recurseR (runFree interp r)
  FixL l -> fixL (runFree interp l)
  FixR r -> fixR (runFree interp r)

liftC :: c a b -> Catalyst r c a b
liftC = LiftC

instance (r ~ 'Req 'HasCategory symP symS monP monS cart cocart rec fix)
         => C.Category (Catalyst r c) where
  id = ID
  (.) = flip Comp

instance (r ~ 'Req 'HasCategory 'HasSymmetricProduct symS 'HasMonoidalProduct monS 'HasCartesian cocart rec fix)
  => C.Cartesian (Catalyst r c) where
  copy = Copy
  consume = Consume
  fst' = Fst
  snd' = Snd

instance (r ~ 'Req 'HasCategory symP 'HasSymmetricSum monP 'HasMonoidalSum cart 'HasCocartesian rec fix)
         => C.Cocartesian (Catalyst r c) where
  injectL = InjectL
  injectR = InjectR
  unify = Unify
  tag = Tag

instance (r ~ 'Req 'HasCategory 'HasSymmetricProduct symS monP monS cart cocart rec fix) => C.SymmetricProduct (Catalyst r c) where
  swap = Swap
  reassoc = Reassoc

instance (r ~ 'Req 'HasCategory symP 'HasSymmetricSum monP monS cart cocart rec fix) => C.SymmetricSum (Catalyst r c) where
  swapE = SwapE
  reassocE = ReassocE

instance (r ~ 'Req 'HasCategory 'HasSymmetricProduct symS 'HasMonoidalProduct monS cart cocart rec fix) => C.MonoidalProduct (Catalyst r c) where
  (***) = Alongside
  first' = First
  second' = Second

instance (r ~ 'Req 'HasCategory symP 'HasSymmetricSum monP 'HasMonoidalSum cart cocart rec fix) => C.MonoidalSum (Catalyst r c) where
  (+++) = EitherOf
  left = Left'
  right = Right'

instance (r ~ 'Req 'HasCategory symP symS monP monS cart cocart 'HasRecursive fix) => Recursive (Catalyst r c) where
  recurseL = RecurseL
  recurseR = RecurseR

instance (r ~ 'Req 'HasCategory symP symS monP monS cart cocart rec 'HasFixed) => Fixed (Catalyst r c) where
  fixL = FixL
  fixR = FixR
