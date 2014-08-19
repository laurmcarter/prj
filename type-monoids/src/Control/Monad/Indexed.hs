{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Control.Monad.Indexed where

import Data.Monoid.Type
import GHC.Exts (Any)

class TMonoid (Any :: k) => IxMonad (m :: k -> * -> *) where
  ireturn :: a -> m M0 a
  ibind   :: m i a -> (a -> m j b) -> m (i <> j) b

type IxMonadVerified (m :: k -> * -> *) = (TMonoidVerified (Any :: k), IxMonad m)

newtype L a = LK a

data instance Sing (x :: L *) where
  L :: a -> Sing (LK a)

newtype Env (as :: [*]) = Env
  { env :: List (Lift as)
  }

type family Lift (as :: [*]) :: [L *] where
  Lift '[]       = '[]
  Lift (a ': as) = LK a ': Lift as

