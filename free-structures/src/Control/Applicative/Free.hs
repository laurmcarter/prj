{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Control.Applicative.Free where

import Control.Applicative

data Free (f :: * -> *) :: * -> * where
  Pure  ::                     x -> Free f x
  (:*:) :: Free f (x -> y) -> f x -> Free f y
infixl 3 :*:

instance Functor (Free f) where
  fmap f t = case t of
    Pure a     -> Pure $ f a
    fxy :*: tx -> fmap f <$> fxy :*: tx

instance Applicative (Free f) where
  pure = Pure
  f <*> x = case f of
    Pure f_   -> f_ <$>  x
    ff :*: fx -> ff <$$> x :*: fx

liftAp :: f a -> Free f a
liftAp fa = Pure id :*: fa

type f :-> g = forall x. f x -> g x

runAp :: Applicative g => (f :-> g) -> Free f a -> g a
runAp f t = case t of
  Pure    x -> pure x
  tf :*: tx -> runAp f tf <*> f tx

hoistAp :: (f :-> g) -> Free f a -> Free g a
hoistAp f t = case t of
  Pure    x -> Pure x
  tf :*: tx -> hoistAp f tf :*: f tx

-- Util {{{

($$) :: (a -> b -> c) -> b -> a -> c
f $$ x = flip f x
infixr 1 $$

(<$$>) :: Applicative f => f (a -> b -> c) -> f b -> f (a -> c)
(<$$>) = liftA2 ($$)
infixl 5 <$$>

-- }}}

