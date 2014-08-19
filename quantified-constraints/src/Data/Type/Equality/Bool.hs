{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Data.Type.Equality.Bool where

import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy
import GHC.Exts (Constraint)

class Decidable (f :: k -> *) where
  (%~) :: f a -> f b -> Cond (a == b) (a :~: b) (a :/: b)

-- Bool {{{

data B (x :: Bool) where
  True_  :: B True
  False_ :: B False

_tf :: True  :/: False
_tf = Void . _tf

_ft :: False :/: True
_ft = Void . _ft

instance Decidable B where
  a %~ b = case (a,b) of
    (True_ ,True_ ) -> T  _T
    (True_ ,False_) -> F _tf
    (False_,True_ ) -> F _ft
    (False_,False_) -> T  _F

_T :: True :~: True
_T = Refl

_F :: False :~: False
_F = Refl

_And :: a :~: c -> b :~: d -> (a && b) :~: (c && d)
_And Refl Refl = Refl

_Or :: a :~: c -> b :~: d -> (a || b) :~: (c || d)
_Or Refl Refl = Refl

type T' = (~)   True
type F' = (~)   False
type T  = (:~:) True
type F  = (:~:) False

conjELF :: Dict (F' (x && y)) -> Cond x (Dict (F' y)) (Dict (F' x),Unless y (Dict (F' y)))
conjELF Dict = TF Dict (Dict,TF () Dict)

conjERF :: Dict (F' (x && y)) -> Cond y (Dict (F' x)) (Dict (F' y),Unless x (Dict (F' x)))
conjERF Dict = TF Dict (Dict,TF () Dict)

{-
conjELT :: Dict ((x && y) ~ True) -> (Dict (x ~ True),Dict (y ~ True))
conjELT Dict = (Dict,Dict)
-}

{-
conjERT :: Dict (F' (x && y)) -> Cond y (Dict (F' x)) (Dict (F' y),Unless x (Dict (F' x)))
conjERT Dict = TF Dict (Dict,TF () Dict)
-}

conjEL'' :: forall p x y. p y -> T' (x && y) -: T' x
conjEL'' _ = Sub $
  let xy = (Dict :: Dict (T' (x && y)))
  in undefined

conjEL :: B x -> B y -> T (x && y) -> T x
conjEL x y Refl = case (x,y) of
  (True_ ,True_ ) -> Refl
  _               -> error "unreachable"

conjER :: B x -> B y -> T (x && y) -> T y
conjER x y Refl = case (x,y) of
  (True_ ,True_ ) -> Refl
  _               -> error "unreachable"

conjIT :: T x -> T y -> T (x && y)
conjIT Refl Refl = Refl

conjIF :: Either (F x) (F y) -> F (x && y)
conjIF e = case e of
  Left  Refl -> Refl
  Right Refl -> Refl

-- }}}

-- Nat {{{

data N
  = Z
  | S N
  deriving (Eq,Show)

type family NatEq (x :: N) (y :: N) :: Bool where
  NatEq  Z     Z    = True
  NatEq  Z    (S y) = False
  NatEq (S x)  Z    = False
  NatEq (S x) (S y) = NatEq x y

type instance x == y = NatEq x y

data Nat (n :: N) where
  Z_ :: Nat Z
  S_ :: Nat n -> Nat (S n)

instance Decidable Nat where
  a %~ b = case (a,b) of
    (Z_   ,Z_   ) -> T  _Z
    (Z_   ,S_ _ ) -> F _zs
    (S_ _ ,Z_   ) -> F _sz
    (S_ a',S_ b') -> (_S *%* _s) (a' %~ b')

_zs :: Z :/: S x
_zs = Void . _zs

_sz :: S x :/: Z
_sz = Void . _sz

_Z :: Z :~: Z
_Z = Refl

_S :: x :~: y -> S x :~: S y
_S Refl = Refl

_S' :: S x :~: S y -> x :~: y
_S' Refl = Refl

_s :: x :/: y -> S x :/: S y
_s f = f . _S'

_s' :: S x :/: S y -> x :/: y
_s' f = f . _S

-- }}}

-- List {{{

data List (f :: k -> *) (as :: [k]) where
  Nil  :: List f '[]
  (:>) :: f a -> List f as -> List f (a ': as)
infixr 4 :>

_Nil :: '[] :~: '[]
_Nil = Refl

_Cons :: a :~: b -> as :~: bs -> (a ': as) :~: (b ': bs)
_Cons Refl Refl = Refl

_cons :: Either (a :/: b) (as :/: bs) -> (a ': as) :/: (b ': bs)
_cons e Refl = case e of
  Left  f -> f Refl
  Right f -> f Refl

_nilCons :: '[] :/: (a ': as)
_nilCons = Void . _nilCons

_consNil :: (a ': as) :/: '[]
_consNil = Void . _consNil

{-
instance Decidable f => Decidable (List f) where
  as %~ bs = case (as,bs) of
    (Nil     ,Nil     ) -> T _Nil
    (Nil     ,b :> bs') -> F _nilCons
    (a :> as',Nil     ) -> F _consNil
    (a :> as',b :> bs') -> _
      where
      ab   = a %~ b
      asbs = as' %~ bs'
-}

-- }}}

-- Cond Type {{{

data Cond (p :: Bool) (t :: *) (f :: *) where
  T  :: t -> Cond True  t f
  F  :: f -> Cond False t f
  TF :: (p ~ True => t) -> (p ~ False => f) -> Cond p t f

condMap :: (p ~ True => a -> c) -> (p ~ False => b -> d) -> Cond p a b -> Cond p c d
condMap f g cnd = case cnd of
  T  a   -> T  (f a)
  F    c -> F        (g c)
  TF a c -> TF (f a) (g c)

(*%*) :: (p ~ True => a -> b) -> (p ~ False => c -> d) -> Cond p a c -> Cond p b d
(*%*) = condMap
infixl 2 *%*

whenMap :: (p ~ True => a -> b) -> Cond p a c -> Cond p b c
whenMap f = condMap f id

(*~*) :: (p ~ True => a -> b) -> Cond p a c -> Cond p b c
(*~*) = whenMap
infixr 3 *~*

unlessMap :: (p ~ False => a -> b) -> Cond p c a -> Cond p c b
unlessMap = condMap id

(*/*) :: (p ~ False => a -> b) -> Cond p c a -> Cond p c b
(*/*) = unlessMap
infixr 3 */*

(*>*) :: Cond q t2 f2 -> Cond p t1 f1
  -> Cond p (Cond q (t1,t2) (t1,f2)) (Cond q (f1,t2) (f1,f2))
(*>*) c1 =
      (\t1 -> ((,) t1) *~* ((,) t1) */* c1)
  *%* (\f1 -> ((,) f1) *~* ((,) f1) */* c1)
infixr 2 *>*

(*<*) :: Cond p t1 f1 -> Cond q t2 f2
  -> Cond p (Cond q (t1,t2) (t1,f2)) (Cond q (f1,t2) (f1,f2))
(*<*) = flip (*>*)
infixl 2 *<*

(*&&*) :: forall p q a b c d. Cond p (a -> c) (b -> d) -> Cond q a b
  -> Cond (p && q) c (Cond p (a -> c,b) (Cond q (b -> d,a) d))
cf *&&* cx = case (cf,cx) of
  (TF f1 f2,T x     ) -> TF (f1 x) $ F $ T (f2,x)
  (TF f1 f2,F x     ) -> F $ TF (f1,x) $ F $ f2 x
  (TF f1 f2,TF x1 x2) -> TF
    (let pq = (Dict :: Dict (T' (p && q)))
     in undefined)
    undefined
  (T f     ,TF x1 x2) -> TF (f x1) $ T (f,x2)
  (F f     ,TF x1 x2) -> F $ F $ TF (f,x1) $ f x2
  (T f,T x) -> T $ f x
  (T f,F x) -> F $ T (f,x)
  (F f,T x) -> F $ F $ T (f,x)
  (F f,F x) -> F $ F $ F $ f x

type When   p a = Cond p a ()
type Unless p a = Cond p () a
type a :/: b    = a :~: b -> Void

-- }}}

-- Misc {{{

data Void = Void !Void

absurd :: Void -> a
absurd a = a `seq` spin a
  where
  spin (Void b) = spin b

vacuous :: Functor f => f Void -> f a
vacuous = fmap absurd

castOuterWith :: (f :~: g) -> f a -> g a
castOuterWith fg = castWith $ fg `apply` Refl

castInnerWith :: (a :~: b) -> f a -> f b
castInnerWith ab = castWith $ Refl `apply` ab

data Dict (c :: Constraint) where
  Dict :: c => Dict c

data (c :: Constraint) -: (d :: Constraint) = Sub (c => Dict d)

(\\) :: (c => r) -> Dict c -> r
r \\ Dict = r
infixl 1 \\

(==>) :: (a -: b) -> (b -: c) -> (a -: c)
Sub ab ==> Sub bc = Sub $ Dict \\ bc \\ ab

-- }}}

