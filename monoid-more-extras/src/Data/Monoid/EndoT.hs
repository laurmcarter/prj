{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Monoid.EndoT where

import Control.Applicative
import Control.Arrow hiding ((***),(&&&))
import Control.Category
import Prelude hiding (id,(.))
import Control.Comonad
import Control.Monad ((>=>),(<=<))
import Control.Lens
import Data.Function (on)
import Data.Monoid
import Data.Monoid.Endomorphism
import Data.Profunctor
import Data.Functor.Adjunction
import qualified Data.Profunctor.Rep as P
import qualified Data.Functor.Rep    as F

newtype Nest k f a = Nest
  { unnest :: Endomorphism k (f a)
  }

instance Category k => Monoid (Nest k f a) where
  mempty  = Nest mempty
  mappend = Nest .: (<>) `on` unnest

(.:) :: (P.Representable p , Category k)
  => k c d -> p a (k b c) -> p a (k b d)
(.:) = under P.tabulated . (~.:)
infixr 4 .:

(~.:) :: (Category k, Functor f) =>
  k c d -> (a -> f (k b c)) -> a -> f (k b d)
(~.:) = (.) . fmap . (.)
infixr 4 ~.:

(.&) :: forall p k a b c.
  (Fabricate p, Category k)
  => k b c -> p (k a b) (k a c)
(.&) bc = embed (bc .)

(^:) :: (Strong p, Category p) => p (b,b) c -> p a b -> p (a,a) c
op ^: f = op . (f *** f)

(***) :: (Strong p, Category p) => p a b -> p c d -> p (a,c) (b,d)
f *** g = first' f . second' g

(&&&) :: (Strong p, Category p) => p a b -> p a c -> p a (b,c)
f &&& g = (f *** g) . dup'
  where

dup' :: (Profunctor p, Category p) => p a (a,a)
dup' = rmap dup id

dup :: a -> (a,a)
dup a = (a,a)

embed :: Fabricate p => (a -> b) -> p a b
embed = P.tabulate . (return .)

coembed :: Cofabricate p => (a -> b) -> p a b
coembed = P.cotabulate . (. extract)

newtype M p a b = M
  { unM :: p a b
  }

_m :: Iso (p a b) (q c d) (M p a b) (M q c d)
_m = iso M unM

type Fabricate p = (P.Representable p, Monad (P.Rep p))

instance Fabricate p => Category (M p) where
  id    = view _M   return
  f . g = view _M $ f.^_M <=< g.^_M

_M :: (P.Representable p, P.Representable q)
  => Iso (a -> P.Rep p b) (c -> P.Rep q d) (M p a b) (M q c d)
_M = P.tabulated . _m

(^.^) :: (forall a b c d. AnIso (f a b) (f c d) (g a b) (g c d))
  -> (f a b -> f c d -> f x y) -> g a b -> g c d -> g x y
(l ^.^ op) f g = view o $ view m f `op` view n g
  where
  m = coerced $ from $ cloneIso l
  n = coerced $ from $ cloneIso l
  o = coerced        $ cloneIso l
infixr 4 ^.^

newtype W p a b = W
  { unW :: p a b
  }

_w :: Iso (p a b) (q c d) (W p a b) (W q c d)
_w = iso W unW

type Cofabricate p = (P.Corepresentable p, Comonad (P.Corep p))

instance Cofabricate p => Category (W p) where
  id    = view _W   extract
  f . g = view _W $ f.^_W =<= g.^_W

_W :: (P.Corepresentable p, P.Corepresentable q)
  => Iso (P.Corep p a -> b) (P.Corep q c -> d) (W p a b) (W q c d)
_W = P.cotabulated . _w

(.^) :: b -> AnIso' a b -> a
(.^) = flip $ view . from

