{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module LambdaPi where

import LambdaPi.Term
import Data.Proxy
import Data.Type.Equality (type (==))
import GHC.TypeLits (Symbol,KnownSymbol,symbolVal)

-- Types {{{

data Value (a :: Term) where
  VStar    :: Value (Star n)
  VPi      :: Value t -> (Value a -> Value t') -> Value (Pi a t t')
  VLam     :: Value t -> (Value a -> Value b)  -> Value (Lam a t b)
  VNeutral :: Neutral a -> Value a

data Neutral (a :: Term) where
  NFree :: Name_ x -> Neutral (F x)
  NApp  :: (a ::: t) => Neutral (Lam a t b) -> Value a -> Neutral b

data Env (as :: [Term]) where
  MTE   :: Env '[]
  (:+) :: Value a -> Env as -> Env (a ': as)
infixr 4 :+

data Cxt (as :: [(Name,Term)]) where
  MTC  :: Cxt '[]
  (:*) :: (Name_ n,Value v) -> Cxt as -> Cxt ('(n,v) ': as)
infixr 4 :*

class Lookup (x :: Name) (g :: [(Name,Term)]) (v :: Term) | x g -> v where
  lookupCxt :: Name_ x -> Cxt g -> Value v

instance ( (x == y) ~ b , Lookup' b x v0 g v) => Lookup x ('(y,v0) ': g) v where
  lookupCxt x ((_,v0) :* g) = lookupCxt' (Proxy :: Proxy b) v0 x g

class Lookup' (b :: Bool) (x :: Name) (v0 :: Term) (g :: [(Name,Term)]) (v :: Term)
  | b x v0 g -> v where
  lookupCxt' :: p0 b -> Value v0 -> Name_ x -> Cxt g -> Value v

instance Lookup' True x v0 g v0 where
  lookupCxt' _ v0 _ _ = v0

instance Lookup x g v => Lookup' False x v0 g v where
  lookupCxt' _ _ = lookupCxt

-- }}}

-- Eval {{{

type e :=> v = Eval e '[] v

eval :: (e :=> v) => Term_ e -> Value v
eval e = eval_ e MTE

class Eval (e :: Term) (g :: [Term]) (v :: Term) where
-- class Eval (e :: Term) (g :: [Term]) (v :: Term) | e g -> v where
  eval_ :: Term_ e -> Env g -> Value v

-- Ann
instance Eval e g v => Eval (Ann e t) g v where
  eval_ (Ann_ e _) = eval_ e

-- Free
instance Eval (F (Global (x :: Symbol))) g (F (Global x)) where
  eval_ (F_ (Global_ p)) _ = VNeutral $ NFree $ Global_ p
  eval_ _ _ = error "come on, GHC"

instance Eval (F (Local i)) g (F (Local i)) where
  eval_ (F_ (Local_ i)) _ = VNeutral $ NFree $ Local_ i
  eval_ _ _ = error "come on, GHC"

{- XXX: needed?
instance Eval (F (Quote i)) g (F (Quote i)) where
  eval_ _ _ = VNeutral $ NFree $ Quote_ Proxy
-}

-- B
instance Eval (B Z) (e ': g) e where
  eval_ _ (e :+ _) = e

instance Eval (B n) g e => Eval (B (S n)) (x ': g) e where
  eval_ (B_ (S_ n)) (_ :+ env) = eval_ (B_ n) env
  eval_ _ _ = error "come on, GHC"

-- Star
instance Eval (Star n) g (Star n) where
  eval_ _ _ = VStar

-- Pi
instance
  ( Eval p g t
  , a ::: t
  , Eval p' (a ': g) t'
  ) => Eval (Pi a p p') g (Pi a t t') where
  eval_ (Pi_ p p') env = VPi (eval_ p env)
    $ \(x :: Value a) -> eval_ p' (x :+ env)

-- Lam
instance
  ( Eval p g t
  , a ::: t
  , Eval e (a ': g) v
  ) => Eval (Lam a p e) g (Lam a t v) where
  eval_ (Lam_ p e) env = VLam (eval_ p env)
    $ \(x :: Value a) -> eval_ e (x :+ env)

-- App
instance
  ( y ::: t
  , Eval f g (Lam y t b)
  , Eval x g y
  ) => Eval (App f x) g b where
  eval_ (App_ f x) env = vapp f' x'
    where
    f' :: Value (Lam y t b)
    f' = eval_ f env
    x' :: Value y
    x' = eval_ x env

vapp :: (a ::: t) => Value (Lam a t b) -> Value a -> Value b
vapp f = case f of
  VLam _ f'  -> f'
  VNeutral n -> VNeutral . NApp n

-- }}}

-- Type {{{

type e ::: t = Type Z e '[] t

typeOf :: (e ::: t) => Term_ e -> Value t
typeOf e = typeOf_ Z_ e MTC

-- class Type (i :: N) (e :: Term) (g :: [(Name,Term)]) (t :: Term) | e g -> t
class Type (i :: N) (e :: Term) (g :: [(Name,Term)]) (t :: Term) where
  typeOf_ :: p0 i -> Term_ e -> Cxt g -> Value t

-- Ann
instance
  ( Type i p g (Star n)
  , p :=> t
  , Type i e g t
  ) => Type i (Ann e p) g t where
  typeOf_ _ (Ann_ e p) env = eval p

-- Star
instance Type i (Star n) g (Star (S n)) where
  typeOf_ _ (Star_ n) _ = VStar

-- Pi
instance
  ( Type i p g (Star n)
  , p :=> t
  , Subst Z (F (Local i)) p' p''
  , Type (S i) p'' ('(Local i,t) ': g) (Star n)
  ) => Type i (Pi x p p') g (Star n) where
  typeOf_ _ (Pi_ p p') env = VStar

-- Free Var
instance Lookup n g t => Type i (F n) g t where
  typeOf_ _ (F_ n) env = lookupCxt n env

-- App
instance
  ( Type i f g (Pi v t t')
  , Type i x g t
  , x :=> v
  ) => Type i (App f x) g t' where
  typeOf_ i (App_ f x) env = case f' of
    VPi t (t' :: Value v -> Value t') -> t' v
    VNeutral _n -> undefined
    where
    f' :: Value (Pi v t t')
    f' = typeOf_ i f env
    v :: Value v
    v = eval x
    v' :: Value t
    v' = typeOf_ i x env

-- Lam
instance
  ( p :=> t
  , Type (S i) e ('(Local i,t) ': g) t'
  ) => Type i (Lam x p e) g (Pi x t t') where
  typeOf_ _ (Lam_ a b) env = undefined

-- }}}

-- Subst {{{

class Subst (i :: N) (r :: Term) (t :: Term) (t' :: Term) | i r t -> t' where
  subst :: Nat i -> Term_ r -> Term_ t -> Term_ t'

instance Subst i r e e' => Subst i r (Ann e t) (Ann e' t) where
  subst i r (Ann_ e t) = Ann_ (subst i r e) t

instance
  ( (i == j) ~ b
  , SubstB b r j t
  ) => Subst i r (B j) t where
  subst _ r (B_ j) = subst' (Proxy :: Proxy b) r j

class    SubstB (b :: Bool) (r :: Term) (j :: N) (t :: Term) | b r j -> t where
  subst' :: proxy b -> Term_ r -> Nat j -> Term_ t
instance SubstB True  r j r where
  subst' _ r _ = r
instance SubstB False r j (B j) where
  subst' _ _ = B_

instance Subst i r (F y) (F y) where
  subst _ r x = x

instance
  ( Subst i r f f'
  , Subst i r x x'
  ) => Subst i r (App f x) (App f' x') where
  subst i r (App_ f x) = App_ (subst i r f) (subst i r x)

instance Subst (S i) r e e' => Subst i r (Lam x t e) (Lam x t e') where
  subst i r (Lam_ t e) = Lam_ t (subst (S_ i) r e)

instance Subst i r (Star n) (Star n) where
  subst i r x = x

instance
  ( Subst i r a a'
  , Subst i r b b'
  ) => Subst i r (Pi x a b) (Pi x a' b') where
  subst i r (Pi_ a b) = Pi_ (subst i r a) (subst i r b)

-- }}}

-- Quote {{{

class Quote (i :: N) (v :: Term) where
  quote_ :: Nat i -> Value v -> Term_ v

instance (Quote i t, Quote (S i) b, x ~ F (Quoted i)) => Quote i (Lam x t b) where
  quote_ i (VLam t f) = Lam_ (quote_ i t)
    $ quote_ (S_ i) $ f $ vfree $ Quoted_ i
  quote_ i (VNeutral n) = undefined

vfree :: Name_ x -> Value (F x)
vfree = VNeutral . NFree

{-
neutralQuote :: Quote i v => Nat i -> Neutral v -> Term_ v
neutralQuote i (NFree (Quoted_ k)) = undefined
-}

boundQuote :: Nat i -> Name_ n -> Term_ (BoundQuote i n)
boundQuote i n = case n of
  Quoted_ k -> B_ _
  Local_  k -> F_ $ Local_ k
  Global_ x -> F_ $ Global_ x

type family BoundQuote (i :: N) (n :: Name) :: Term where
  BoundQuote (S i) (Quoted k) = B (i - k)
  BoundQuote (S i) (Local  k) = F (Local  k)
  BoundQuote (S i) (Global x) = F (Global x)

type family (x :: N) - (y :: N) :: N where
  x   - Z   = x
  Z   - y   = Z
  S x - S y = x - y

-- }}}

{-
e0 :: Term_ (Lam x a (B Z))
e0 = Lam_ $ B_ Z_
-}

{-
type T0 = forall t. (E0 ::: t) => t
-}

