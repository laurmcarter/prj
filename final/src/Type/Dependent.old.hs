{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module Type.Dependent where

import GHC.Exts (Any)
import Prelude hiding (pi,sum)
import Data.Proxy
import Data.Type.Bool

type family Lam  (r :: * -> * -> *) :: * -> * -> *
type family Pi   (r :: * -> * -> *) :: * -> * -> * -> *
type family Star (r :: * -> * -> *) :: N -> *

class Dep r where
  lam  ::                     (r x t  ->  r e t')                    -> r (Lam r x e)      (Pi r x t t')
  app  :: r (Lam r x e) (Pi r x t t') ->  r x t                      -> r e t'
  ann  :: r v t                       ->  r t k                      -> r v t
  star ::                                                               r (Star r i)       (Star r (S i))
  pi   :: Max i j k => r t (Star r i) -> (r x t ->  r t' (Star r j)) -> r (Pi r x t t')    (Star r k)
  arr  :: Max i j k => r t (Star r i)           -> (r t' (Star r j)) -> r (Pi r x t t')    (Star r k)
  arr t t' = pi t $ const t'

newtype R v t = R
  { eval :: t
  }

{-
type instance Lam  R = (->)
type instance Pi   R = Any
-}

{-
instance Dep R where
  lam f = R $ _
-}

-- Predicativity {{{

-- type  T r v t = TermEq v t False => r v t
class    TermEq (x :: *) (y :: *) (b :: Bool) | x y -> b
instance                TermEq x x True
instance (False ~ r) => TermEq x y r

class    Max (i :: N) (j :: N) (r :: N) | i j -> r
instance              Max  Z     Z     Z
instance              Max  Z    (S i) (S i)
instance              Max (S i)  Z    (S i)
instance Max i j r => Max (S i) (S j) (S r)

-- }}}

-- Type level specification {{{

class DepNat r => DepLevel r where
  starAt :: r (Val r i) (Type r N) -> r (Star r i) (Star r (S i))

-- }}}

type family Val  (r :: * -> * -> *)          :: k -> *
type family Type (r :: * -> * -> *) (a :: *) :: *

{-
t0 :: Dep r => r (Lam r t' (Lam r e e)) (Pi r t' k (Pi r e t' t'))
t0 = lam $ \a -> lam $ \x -> ann x a

t1 :: (Dep r, Max (S i) j k, Max i i j)
  => r (Pi r x (Star r i) (Pi r x1 x x)) (Star r k)
t1 = pi star $ \a -> arr a a

t2 :: (Dep r, Max (S i) j k, Max i i j)
  => r (Lam r t' (Lam r e e)) (Pi r t' (Star r i) (Pi r e t' t'))
t2 = ann t0 t1

t3 :: (Dep r, DepNat r) => r (Val r (S Z)) (Type r N)
t3 = s z
-}

-- t4 :: (DepVec r, Dep r)
--   => r (VCons r (Val r Z) (VNil r)) (Vec r (S Z) (Val r N))
-- t4 :: (Dep r, DepVec r, Val r ~ Vec r 'Z, VNil r ~ Vec r 'Z b)
--   => r (Vec r 'Z (VCons r (Val r 'Z) b)) (Vec r ('S 'Z) (Vec r 'Z N))
-- t4 = vcons z vnil `ann` vec (s z) nat

-- Prod {{{

type family Prod     (r :: * -> * -> *) :: * -> * -> *
type family ProdType (r :: * -> * -> *) :: * -> * -> * -> *

class DepProd r where
  (&)     :: r x t                       -> r y t'                         -> r (Prod r x y)     (ProdType r x t t')
  sigma   :: Max i j k => r t (Star r i) -> (r x t    ->  r t' (Star r j)) -> r (ProdType r x t t') (Star r k)
  letProd :: r (Prod r x y) (ProdType r x t t') -> (r x t -> r y t' -> r z u) -> r z u

-- }}}

-- Sum {{{

type family Sum     (r :: * -> * -> *) :: * -> * -> *
type family SumType (r :: * -> * -> *) :: * -> * -> *

class DepSum r where
  sum :: Max i j k => r t (Star r i) -> r t' (Star r j) -> r (SumType r t t') (Star r k)
  inl :: r x t  -> r (Val r (Left  x)) (SumType r t t')
  inr :: r y t' -> r (Val r (Right y)) (SumType r t t')
  caseSum :: r (Val r e) (SumType r t t') -> (r x t -> r a u) -> (r y t' -> r b u) -> r (PrjSum e a b) u

type family PrjSum (e :: Either * *) (x :: *) (y :: *) :: * where
  PrjSum (Left a)  c d = c
  PrjSum (Right b) c d = d

-- }}}

-- Wellordering {{{

type family WellOrd (r :: * -> * -> *) :: * -> * -> * -> *
type family Sup     (r :: * -> * -> *) :: * -> * -> *

class DepWellOrd r where
  wellOrd :: Max i j k => r t (Star r i) -> (r x t -> r t' (Star r j)) -> r (WellOrd r x t t') (Star r k)
  sup     :: r x t -> r y (Pi r x t' (WellOrd r x t t')) -> r (Sup r x y) (WellOrd r x t t')

-- }}}

-- Boolean {{{

class DepBool r where
  bool :: r (Type r Bool) (Star r Z)
  tt   :: r (Val  r True)  (Type r Bool)
  ff   :: r (Val  r False) (Type r Bool)

-- }}}

-- Natural {{{

data N
  = Z
  | S N

type family Add (r :: * -> * -> *) :: N -> N -> N

class DepNat r where
  nat  :: r (Type r N) (Star r Z)
  z    :: r (Val  r Z) (Type r N)
  s    :: r (Val  r n) (Type r N) -> r (Val r (S n)) (Type r N)
  add  :: r (Val  r m) (Type r N) -> r (Val r n) (Type r N) -> r (Val r (Add r m n)) (Type r N)

{-
type instance Star R   = Const ()
type instance Type R N = Int
-}

{-
instance DepNat R where
  nat     = R $ Const ()
  z       = R 0
  s n     = R $ 1 + eval n
  add x y = R $ eval x + eval y
-}

t4 :: DepNat r => r (Val r (Add r (S (S Z)) (S (S (S Z))))) (Type r N)
t4 = add (s (s z)) (s (s (s z)))
  

-- }}}

-- List {{{

data List (t :: *)
data Nil
data Cons (a :: *) (b :: *)

class DepList r where
  list  :: r t (Star r i) -> r (List t) (Star r i)
  nil  :: r (Val r Nil) (Val r (List t))
  cons :: r a t -> r (Val r b) (Val r (List t)) -> r (Val r (Cons a b)) (Val r (List t))

-- }}}

-- Vector {{{

type family Vec   (r :: * -> * -> *) :: N -> * -> *
type family VNil  (r :: * -> * -> *) :: *
type family VCons (r :: * -> * -> *) :: * -> * -> *

class DepNat r => DepVec r where
  vec   :: r (Val r n) (Val r N) -> r t (Star r i) -> r (Vec r n t) (Star r i)
  vnil  :: r (VNil r) (Val r (Vec r Z t))
  vcons :: r a t -> r (Val r b) (Vec r n t) -> r (Val r (VCons r a b)) (Vec r (S n) t)

-- }}}

-- Identity Type {{{

data EQ (t :: *) (a :: *) (b :: *)
data Refl

class DepId r where
  (=:) :: Max i j k => r t (Star r i) -> r a t -> r b t -> r (EQ t a b) (Star r i)
  refl :: r Refl (EQ t a a)

-- }}}

