{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Data.Composition where

import Prelude hiding (id, (.))

import Control.Category (Category)
import Control.Applicative
import Control.Lens
import Data.Bifunctor
import Data.Semigroupoid
import Data.Profunctor
import "profunctors" Data.Profunctor.Composition

(.) :: (Category p, Profunctor p) => p b c -> p a b -> p a c
pbc . pab = procomposed ppac
  where
  ppac = Procompose pab pbc

(.:) :: (Category p, Profunctor p) => p c d -> p a (p b c) -> p a (p b d)
(.:) = rmap . (.)

(.:.) :: (Category p, Profunctor p) => p d e -> p a (p b (p c d)) -> p a (p b (p c e))
(.:.) = rmap . (.:)

c1 :: (b -> c) -> (a -> b) -> a -> c
c1 = (.)

c1_2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
c1_2 = c1 . c1

c1_3 :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
c1_3 = c1_2 . c1

c2 :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
c2 = liftA2

c2_2 :: (c -> d -> e) -> (a -> b -> c) -> (a -> b -> d) -> a -> b -> e
c2_2 = c2 . c2

c2_3 :: (d -> e -> f) -> (a -> b -> c -> d) -> (a -> b -> c -> e) -> a -> b -> c -> f
c2_3 = c2_2 . c2

c3 :: (b -> c -> d -> e) -> (a -> b) -> (a -> c) -> (a -> d) -> a -> e
c3 = liftA3

c3_2 :: (c -> d -> e -> f) -> (a -> b -> c) -> (a -> b -> d) -> (a -> b -> e) -> a -> b -> f
c3_2 = c3 . c3

c3_3 :: (d -> e -> f -> g) -> (a -> b -> c -> d) -> (a -> b -> c -> e) -> (a -> b -> c -> f) -> a -> b -> c -> g
c3_3 = c3_2 . c3


{-
-- Can't be written, since Bifunctor doesn't have any introductions
--  defined by the class.
foo :: forall p q a b c d e f. (Category p, Profunctor p, Bifunctor q)
  => Iso (p a (p b c)) (p d (p e f)) (p (q a b) c) (p (q d e) f)
foo = iso crry uncrry
  where
  crry :: p a (p b c) -> p (q a b) c
  crry = undefined
  uncrry :: p (q d e) f -> p d (p e f)
  uncrry = undefined
-}

