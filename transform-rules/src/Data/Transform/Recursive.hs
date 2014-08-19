{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module Data.Transform.Recursive where

import Control.Applicative
import Control.Lens
import Control.Monad ((>=>))

type Recursion s t a b  = forall f. (Applicative f, Monad f)
  => LensLike f s t a b
type Recursion' s a     = Recursion s s a a
type Under      s   a b = Recursion s s a b

bottomUp :: (Plated s, Applicative f, Monad f) => LensLike f s s a b -> LensLike f s s a b
bottomUp = bottomUpOf plate

topDown :: (Plated s, Applicative f, Monad f) => LensLike f s s a b -> LensLike f s s a b
topDown = topDownOf plate

bottomUpOf :: forall f s a b. (Applicative f, Monad f) => Traversal' s s -> LensLike f s s a b -> LensLike f s s a b
bottomUpOf r t (f :: a -> f b) = go
  where
  go = r go >=> t f

bottomUpOf' :: forall f s t a b. (Applicative f, Monad f) => Traversal s t s t -> LensLike f t t a b -> LensLike f s t a b
bottomUpOf' (r :: forall g. Applicative g => (s -> g t) -> s -> g t) t (f :: a -> f b) = go
  where
  go = rec >=> here
  ----
  rec :: s -> f t
  rec = r go
  ----
  here :: t -> f t
  here = t f

topDownOf :: forall f s t a b. (Applicative f, Monad f) => Traversal s t s t -> LensLike f s s a b -> LensLike f s t a b
topDownOf r t f = go
  where
  go   :: s -> f t
  go   = here >=> rec
  ----
  here :: s -> f s
  here = t f
  ----
  rec :: s -> f t
  rec  = r go

(<+>) :: (Conjoined p, Applicative f)
  => Traversing p f s t a b
  -> Traversing p f s t a b
  -> Over p f s t a b
(<+>) = failing
infixr 4 <+>

