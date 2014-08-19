{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Categorical.Bifunctor where

import qualified Prelude
import Prelude hiding (id,(.))

import Data.Semigroupoid
import Control.Categorical
import Control.Category.Dual

class (Semigroupoid r, Semigroupoid t)
  => PFunctor (p :: k -> k -> l) (r :: k -> k -> *) (t :: l -> l -> *)
  | p r -> t, p t -> r where
  first :: r a b -> t (p a c) (p b c)

class (Semigroupoid s, Semigroupoid t)
  => QFunctor (q :: k -> k -> l) (s :: k -> k -> *) (t :: l -> l -> *)
  | q s -> t, q t -> s where
  second :: s a b -> t (q c a) (q c b)

class (PFunctor p r t, QFunctor p s t)
  => Bifunctor (p :: k -> k -> l)
       (r :: k -> k -> *) (s :: k -> k -> *)
       (t :: l -> l -> *)
  | p r -> s t, p s -> r t, p t -> r s where
  bimap :: r a b -> s c d -> t (p a c) (p b d)

dimap :: Bifunctor f (Dual r) s t => r b a -> s c d -> t (f a c) (f b d)
dimap = bimap . Dual

difirst :: PFunctor f (Dual r) t => r b a -> t (f a c) (f b c)
difirst = first . Dual

disecond :: QFunctor f (Dual s) t => s b a -> t (f c a) (f c b)
disecond = second . Dual

instance PFunctor (,) (->) (->) where
  first f = bimap f id
instance QFunctor (,) (->) (->) where
  second  = bimap id
instance Bifunctor (,) (->) (->) (->) where
  bimap f g (a,b) = (f a,g b)

instance PFunctor Either (->) (->) where
  first f = bimap f id
instance QFunctor Either (->) (->) where
  second  = bimap id
instance Bifunctor Either (->) (->) (->) where
  bimap f g = \case
    Left  a -> Left  $ f a
    Right b -> Right $ g b

instance QFunctor (->) (->) (->) where
  second = (.)

instance PFunctor f r t => PFunctor f (Dual r) (Dual t) where
  first = Dual . first . runDual

instance QFunctor f s t => QFunctor f (Dual s) (Dual t) where
  second = Dual . second . runDual

instance Bifunctor f r s t => Bifunctor f (Dual r) (Dual s) (Dual t) where
  bimap f g = Dual $ bimap (runDual f) (runDual g)

