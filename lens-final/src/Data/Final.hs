{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Final where

import Data.Proxy
import GHC.Exts (Constraint)

data family Initial (c :: k -> Constraint) :: k -> *

class Foo r where
  foo      :: Int -> Bool -> r

data instance Initial Foo r = Foo Int Bool

instance Foo Int where
  foo i _ = i

instance Foo (Initial Foo r) where
  foo = Foo

class Match (c :: k -> Constraint) (r :: k) (b :: Bool) | c r -> b where
  type Matched   c r b :: *
  type Unmatched c r b :: *
  match :: c r => Proxy c -> Unmatched c r b -> Either (Matched c r b) (Unmatched c r b)

instance Match Foo (Initial Foo r) True where
  type Unmatched Foo (Initial Foo r) True = r
  type Matched   Foo (Initial Foo r) True = Initial Foo r
  match _ = Right

instance (b ~ False, Foo r) => Match Foo r b where
  type Unmatched Foo r b = r
  type Matched   Foo r b = r
  match _ = Left

