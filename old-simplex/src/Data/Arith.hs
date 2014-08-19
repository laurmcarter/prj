{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Arith where

import Prelude hiding (succ, pred, max)

import Data.Proxy
import Data.Reflection
import GHC.TypeLits

-- Z    : 0
-- D  n : 2n
-- SD n : 2n+1
-- PD n : 2n-1

data B
  = B0
  | B1
  | B :*: B
  deriving (Eq,Show)

infixl 4 :*:

-- Nat{0,1} {{{

class Nat0 (n :: k) where
  toInt :: Proxy n -> Int
class Nat0 n => Nat1 n

class Nat0 n => Nat0E n
instance Nat0 n => Nat0E n
class Nat1 n => Nat1E n
instance Nat1 n => Nat1E n

instance Nat0 B0 where
  toInt _ = 0
instance Nat0 B1 where
  toInt _ = 1
instance Nat1 n => Nat0 (n :*: B0) where
  toInt _ = 2 * toInt (Proxy :: Proxy n)
instance Nat1 n => Nat0 (n :*: B1) where
  toInt _ = 2 * toInt (Proxy :: Proxy n) + 1

instance Nat1 B1
instance Nat1 n => Nat1 (n :*: B0)
instance Nat1 n => Nat1 (n :*: B1)

instance Nat0 Z where
  toInt _ = 0
instance (Nat0 n) => Nat0 (SD n) where
  toInt _ = 2 * toInt (Proxy :: Proxy n) + 1
instance (Nat1 n) => Nat0 (D n) where
  toInt _ = 2 * toInt (Proxy :: Proxy n)

instance Nat1 (SD Z)
instance (Nat0 n) => Nat1 (SD n)
instance (Nat1 n) => Nat1 (D n)

-- }}}

-- Succ {{{

class (Nat0 m, Nat1 n) => Succ m n | m -> n, n -> m

instance Succ B0 B1
instance Succ B1 (B1 :*: B0)
instance Nat1 n => Succ (n :*: B0) (n :*: B1)
instance (Nat1 m, Succ m (n :*: d)) => Succ (m :*: B1) (n :*: d :*: B0)

-- succ 0 = 1 + 2 * 0
instance Succ Z (SD Z)
-- succ (2 * n + 1) = 2 (n + 1)
instance (Nat0 n, Succ m n) => Succ (SD m) (D n)
-- succ (2 * n) = (2 * n + 1)
instance (Nat1 n, Succ m n) => Succ (D n) (SD n)

succ :: Succ m n => proxy m -> Proxy n
succ _ = Proxy
pred :: Succ m n => proxy n -> Proxy n
pred _ = Proxy

-- }}}

-- Add {{{

class (Nat0 x, Nat0 y, Nat0 z) => Add' x y z | x y -> z, x z -> y
instance Nat0 y => Add' B0 y y
instance Succ y z => Add' B1 y z
instance (Nat1 x, Nat1 (z :*: dz), Add' x y' z, AddC y' dz y)
  => Add' (x :*: B0) y (z :*: dz)
instance (Nat1 x, Nat0 z, Add' x y' zh, AddC y' dy y, Succ (zh :*: dy) z)
  => Add' (x :*: B1) y z

class (Nat0 xh, Nat0 x) => AddC xh d x | xh d -> x, x -> xh d
instance AddC B0 B0 B0
instance AddC B0 B1 B1
instance (Nat0 (B1 :*: d)) => AddC B1 d (B1 :*: d)
instance (Nat0 (x :*: d), Nat0 (x :*: d :*: d'))
  => AddC (x :*: d) d' (x :*: d :*: d')

class (Add' x y z, Add' y x z) => Add x y z | x y -> z, z x -> y, z y -> x
instance (Add' x y z, Add' y x z) => Add x y z

add :: Add x y z => proxy x -> proxy y -> Proxy z
add _ _ = Proxy
sub :: Add x y z => proxy z -> proxy x -> Proxy y
sub _ _ = Proxy

-- }}}

-- NCompare {{{

data Order
  = BLT
  | BEQ
  | BGT
  deriving (Eq,Show)

class (Nat0 x, Nat0 y) => NCompare (x :: B) (y :: B) (r :: Order) | x y -> r
instance NCompare B0 B0 BEQ
instance NCompare B0 B1 BLT
instance Nat1 (y :*: dy) => NCompare B0 (y :*: dy) BLT
instance NCompare B1 B0 BGT
instance NCompare B1 B1 BEQ
instance Nat1 (y :*: dy) => NCompare B1 (y :*: dy) BLT
instance Nat1 (x :*: dx) => NCompare (x :*: dx) B0 BGT
instance Nat1 (x :*: dx) => NCompare (x :*: dx) B1 BGT
instance (Nat1 (x :*: dx), Nat1 (y :*: dy), NCompare dx dy dr, NCompare x y r',
          NCS r' dr r)
  => NCompare (x :*: dx) (y :*: dy) r

class NCS r1 r2 r3 | r1 r2 -> r3
instance NCS BEQ r r
instance NCS BGT r BGT
instance NCS BLT r BLT

cmp :: NCompare x y r => proxy x -> proxy y -> Proxy r
cmp _ _ = Proxy

-- }}}

-- NLessEq {{{

class (Nat0 x, Nat0 y) => NLessEq x y
instance NLessEq B0 B0
instance NLessEq B0 B1
instance Nat1 (y :*: dy) => NLessEq B0 (y :*: dy)
instance NLessEq B1 B1
instance Nat1 (y :*: dy) => NLessEq B1 (y :*: dy)
instance (Nat1 x, Nat1 y, NLessEq x y) => NLessEq (x :*: B0) (y :*: B0)
instance (Nat1 x, Nat1 y, NLessEq x y) => NLessEq (x :*: B1) (y :*: B1)
instance (Nat1 x, Nat1 y, NLessEq x y) => NLessEq (x :*: B0) (y :*: B1)
instance (Nat1 x, Nat1 y, NCompare x y BLT) => NLessEq (x :*: B1) (y :*: B0)

assert_leq :: NLessEq x y => proxy x -> proxy y -> ()
assert_leq _ _ = ()

-- }}}

-- NMax {{{

class NMax x y b r | x y b -> r
instance NMax x y BLT y
instance NMax x y BEQ y
instance NMax x y BGT x

max :: (NCompare x y b, NMax x y b r) => proxy x -> proxy y -> Proxy r
max _ _ = Proxy

-- }}}

-- Mul {{{

class (Nat1 x, Nat0 y, Nat0 z) => Mul' x y z | x y -> z, x z -> y
instance Nat0 y => Mul' B1 y y
instance (Mul' x y zh, AddC zh B0 z) => Mul' (x :*: B0) y z
instance (Mul'F x y z, Mul'B x y z) => Mul' (x :*: B1) y z

class (Nat1 x, Nat0 y, Nat0 z) => Mul'F x y z | x y -> z

instance Nat1 x => Mul'F x B0 B0
instance Nat1 x => Mul'F x B1 (x :*: B1)

instance (Mul'F x y z, Nat1 x, Nat1 y, Nat1 z) => Mul'F x (y :*: B0) (z :*: B0)

instance (Mul'F x y z', Add x z' z, Nat1 x, Nat1 y, Nat1 z)
  => Mul'F x (y :*: B1) (z :*: B1)

class (Nat1 x, Nat0 y, Nat0 z) => Mul'B x y z | z x -> y
instance Nat1 x => Mul'B x B0 B0

instance (Mul'B x y z, Nat1 x, Nat1 y, Nat1 z) => Mul'B x (y :*: B0) (z :*: B0)

instance (AddC y B1 yt, Mul'B x y z', Add x z' z, Nat1 x, Nat1 z)
  => Mul'B x yt (z :*: B1)

class (Mul' x y z, Mul' y x z) => Mul x y z | x y -> z, x z -> y, y z -> x
instance (Mul' x y z, Mul' y x z) => Mul x y z

mul :: Mul x y z => proxy x -> proxy y -> Proxy z
mul _ _ = Proxy

div :: Mul x y z => proxy z -> proxy x -> Proxy y
div _ _ = Proxy

-- }}}

-- Fac {{{

class (Nat0 x, Nat1 y) => Fac x y | x -> y, y -> x

instance Fac B1 B1
instance
  ( Nat1 (y :*: dy)
  , Fac w r
  , Succ w (x :*: dx)
  , Mul (x :*: dx) r (y :*: dy)
  ) => Fac (x :*: dx) (y :*: dy)

fac :: Fac x y => proxy x -> Proxy y
fac _ = Proxy

-- }}}

