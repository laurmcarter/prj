{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Unify.Fix where

import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Traversable as T

type family Base t :: * -> *
type instance Base (Mu f) = f

type family LP (lv :: * -> *) (fs :: [* -> *]) (cs :: [*])
type instance LP lv fs cs = Mu (LP' lv fs cs)

type family LP' (lv :: * -> *) (fs :: [* -> *]) (cs :: [*]) :: * -> *
type instance LP' lv fs (c ': cs) = Const c :+: (LP' lv fs cs)
type instance LP' lv fs '[] = LP'' lv fs

type family LP'' (lv :: * -> *) (fs :: [* -> *]) :: * -> *
type instance LP'' lv (f ': l) = f :+: (LP'' lv l)
type instance LP'' lv '[] = lv

-- Base {{{

newtype Mu f = In (f (Mu f))

out :: Mu f -> f (Mu f)
out (In e) = e

data (f :+: g) e = Inl (f e) | Inr (g e) deriving (Show,Functor,F.Foldable,T.Traversable)

infixr :+:

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance (Functor f) => f :<: f where
  inj = id
  prj = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  prj cp = case cp of
    Inl e -> Just e
    Inr _ -> Nothing

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  prj cp = case cp of
    Inl _ -> Nothing
    Inr e -> prj e

inject :: (f :<: g) => f (Mu g) -> Mu g
inject = In . inj

match :: (g :<: f) => Mu f -> Maybe (g (Mu f))
match (In e) = prj e

foldMu :: Functor f => (f a -> a) -> Mu f -> a
foldMu f (In e) = f (fmap (foldMu f) e)

foldMuM :: (Applicative m, Monad m, T.Traversable f) => (f a -> m a) -> Mu f -> m a
foldMuM f (In e) = T.traverse (foldMuM f) e >>= f

--foldMu2 :: Functor f => (f a -> f a -> a) -> Mu f -> a
--foldMu2 f (In x) (In y) = 

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(.::) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.::) = (.) . (.:)

-- }}}

{-
-- Algebra {{{

class (Functor f) => Algebra a f where
  alg :: f a -> a
  evalAlg :: Mu f -> a
  evalAlg = foldMu alg
  evalAlg' :: Algebra a g => f (Mu g) -> a
  evalAlg' = alg . fmap evalAlg

instance (Algebra a f, Algebra a g) => Algebra a (f :+: g) where
  alg cp = case cp of
    Inl e -> alg e
    Inr e -> alg e

-- }}}

-- Match {{{

class (F.Foldable f, T.Traversable f) => Algebra2 a f where
  alg2 :: f a -> f a -> a
  --evalAlg2 :: Mu f -> Mu f -> a
  --evalAlg2 f (In x) (In y) = foldl reduceRes emptyRes $ alg2 x y

class (F.Foldable f, T.Traversable f, Algebra String f) => Match f where
  matchM :: (MonadPlus m, Algebra String g) => (Mu g -> Mu g -> m ()) -> f (Mu g) -> f (Mu g) -> m ()

instance (Match f, Match g) => Match (f :+: g) where
  matchM f cp1 cp2 = case (cp1,cp2) of
    (Inl e1,Inl e2) -> matchM f e1 e2
    (Inr e1,Inr e2) -> matchM f e1 e2
    _               -> fail ("Type mismatch: " ++ evalAlg' cp1 ++ " and " ++ evalAlg' cp2)

-- }}}
-}

-- Equal {{{

class (Functor f) => Equal f where
  equal :: (Equal g) => f (Mu g) -> f (Mu g) -> Bool

instance (Equal f, Equal g) => Equal (f :+: g) where
  equal cp1 cp2 = case (cp1,cp2) of
    (Inl e1,Inl e2) -> equal e1 e2
    (Inr e1,Inr e2) -> equal e1 e2
    _               -> False

instance (Equal f) => Eq (Mu f) where
  (In e1) == (In e2) = equal e1 e2

-- }}}

-- Parse {{{

class (Functor f) => Parse f where
  parsesPrec :: Parse g => Int -> ReadS (f (Mu g))

-- Causing duplication
instance (Parse f, Parse g) => Parse (f :+: g) where
  parsesPrec x s = (lmap $ parsesPrec x s) ++ (rmap $ parsesPrec x s)
    where
    lmap = map $ \(a,s) -> (Inl a,s)
    rmap = map $ \(a,s) -> (Inr a,s)

instance Parse f => Read (Mu f) where
  readsPrec x s = map (\(a,s)->(In a,s)) $ parsesPrec x s

-- }}}

-- Render {{{

class (Functor f) => Render f where
  render :: Render g => f (Mu g) -> String

instance (Render f, Render g) => Render (f :+: g) where
  render cp = case cp of
    Inl e -> render e
    Inr e -> render e

instance Render f => Show (Mu f) where
  show (In e) = render e

-- }}}

