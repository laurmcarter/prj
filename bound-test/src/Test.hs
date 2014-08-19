{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Test where

import Bound
import Prelude.Extras

import Data.Constraint.All
import Data.Constraint.Member
import Data.Constraint.Dict
import Control.Applicative
import Control.Monad (ap)
import Control.Monad.Free
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import GHC.Prim (Constraint)
import Data.Proxy

type U f = Free f

-- Injection {{{

data (f :+: g) a
  = InL (f a)
  | InR (g a)
  deriving (Eq,Show,Functor,Foldable,Traversable)
infixr 4 :+:

instance (Eq1 f, Eq1 g) => Eq1 (f :+: g) where
  s1 ==# s2 = case (s1,s2) of
    (InL a,InL b) -> a ==# b
    (InR a,InR b) -> a ==# b
    _             -> False

class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a
  prj :: g a -> Maybe (f a)

class (Functor f, Functor g) => InjSum (p :: Bool) f g where
  inj_ :: Proxy p -> f a -> g a
  prj_ :: Proxy p -> g a -> Maybe (f a)

instance Functor f => f :<: f where
  inj = id
  prj = Just

instance
  ( Functor f, Functor g, Functor h
  , IsSum p g, InjSum p f (g :+: h)
  ) => f :<: (g :+: h) where
  inj = inj_ (Proxy :: Proxy p)
  prj = prj_ (Proxy :: Proxy p)

instance
  ( Functor g, Functor h, Functor i
  , f :<: (g :+: h :+: i)
  ) => InjSum True f ((g :+: h) :+: i) where
  inj_ _ t = case inj t of
    InL t'       -> InL $ InL t'
    InR (InL t') -> InL $ InR t'
    InR (InR t') -> InR t'
  prj_ _ s = prj $ case s of
    InL (InL e) -> InL e
    InL (InR e) -> InR $ InL e
    InR e       -> InR $ InR e

instance (Functor f, Functor g) => InjSum False f (f :+: g) where
  inj_ _ = InL
  prj_ _ s = case s of
    InL e -> Just e
    InR _ -> Nothing

instance (f :<: h, Functor g) => InjSum False f (g :+: h) where
  inj_ _ = InR . inj
  prj_ _ s = case s of
    InL _ -> Nothing
    InR e -> prj e

class IsSum (p :: Bool) (f :: * -> *) | f -> p
instance (True  ~ p) => IsSum p (f :+: g)
instance (False ~ p) => IsSum p f

-- }}}

-- Helpers {{{

inject :: (f :<: g) => f (U g a) -> U g a
inject = Free . inj

injectFree :: (f :<: g) => f a -> U g a
injectFree = inject . fmap Pure

injectPure :: a -> U f a
injectPure = Pure

matchPure :: U g a -> Maybe a
matchPure (Pure a) = Just a
matchPure _        = Nothing

matchFree :: (f :<: g) => U g a -> Maybe (f (U g a))
matchFree t = case t of
  Free f -> prj f
  Pure _ -> Nothing

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 8 .:

-- }}}

var :: a -> U f a
var = Pure

-- LC {{{

data LC ls rs pre pst r
  = forall f a.
    ( r ~ U f a
    , Each ls f
    , Each rs a
    , Each pre r
    ) => Lam (DictList1 pst (U f a)) (Scope () (U f) a)
  | r :@ r
infixr 9 :@



foo :: (Each ls f, Each rs a, Each pre (U f a)) => DictList1 pst (U f a) -> Scope () (U f) a -> LC ls rs pre pst (U f a)
foo = Lam

{-
instance ((~) U f b ∈ posts) => Functor (LC l r c) where
  
-}

{-
data LC r where
  Lam  :: (Functor f, Eq1 f, Eq a) => Scope () (U f) a -> LC (U f a)
  (:@) :: r -> r -> LC r
infixr 9 :@

instance Eq1 LC where
  Lam a    ==# Lam b    = a == b
  (f :@ a) ==# (g :@ b) = f == g && a == b
  _        ==# _        = False

instance Functor LC where
  fmap (f :: a -> b) (Lam (b :: Scope () (U f) a')) = undefined
  fmap f (a :@ b) = f a :@ f b

lam :: (LC :<: f, Eq a, Eq1 f)
  => a -> U f a -> U f a
lam v = inject . Lam . abstract1 v

(@:) :: (LC :<: f) => U f a -> U f a -> U f a
f @: a = inject $ f :@ a
infixr 9 @:

whnf :: (LC :<: f)
  => U f a -> U f a
whnf (matchFree -> Just (f :@ a)) = case matchFree f' of
  Just (Lam b) -> whnf $ instantiate1 a b
  _            -> f' @: a
  where
  f' = whnf f
whnf e = e
-}

-- }}}

-- Arith {{{

data Number r
  = r :+ r
  | r :* r
  | Negate r
  | Abs r
  | Signum r
  | I Integer
  deriving (Eq,Show,Functor,Foldable,Traversable)
infixl 6 :+
infixl 7 :*

instance Eq1 Number

instance (Number :<: f) => Num (U f a) where
  (+)         = inject .: (:+)
  (*)         = inject .: (:*)
  negate      = inject . Negate
  abs         = inject . Abs
  signum      = inject . Signum
  fromInteger = inject . I

{-
type Exp = U (LC :+: Number) Int

e1 :: Exp
e1 = var 0

e2 :: Exp
e2 = lam 0 e1
-}

-- }}}

