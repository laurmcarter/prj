{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Nat.Binary where

import Prelude hiding (max,div)

import Data.Proxy
import Data.Tagged

data BN
  = B0
  | B1
  | BN :& BN
  deriving (Eq,Show)

infixl 4 :&

-- Low Dimension {{{

type Zero  = B0
type One   = B1
type Two   = B1 :& B0
type Three = B1 :& B1
type Four  = B1 :& B0 :& B0
type Five  = B1 :& B0 :& B1
type Six   = B1 :& B1 :& B0

zeroBP  :: Proxy Zero
zeroBP  = Proxy
oneBP   :: Proxy One
oneBP   = Proxy
twoBP   :: Proxy Two
twoBP   = Proxy
threeBP :: Proxy Three
threeBP = Proxy
fourBP  :: Proxy Four
fourBP  = Proxy
fiveBP  :: Proxy Five
fiveBP  = Proxy
sixBP  :: Proxy Six
sixBP  = Proxy

-- }}}

-- Well Formed {{{

shiftP :: p (x :& y) -> Proxy x
shiftP _ = Proxy

class Nat0 (n :: BN) where
  ncata :: a -> a -> (a -> a) -> (a -> a) -> Proxy n -> a
class Nat0 n => Nat n

instance Nat0 B0 where
  ncata z _ _ _ _ = z
instance Nat0 B1 where
  ncata _ o _ _ _ = o
instance Nat x => Nat0 (x :& B0) where
  ncata z o f fp = f . ncata z o f fp . shiftP
instance Nat x => Nat0 (x :& B1) where
  ncata z o f fp = fp . ncata z o f fp . shiftP

instance Nat B1
instance Nat x => Nat (x :& B0)
instance Nat x => Nat (x :& B1)

nat :: (Integral l, Nat0 n) => Proxy n -> l
nat = ncata 0 1 (*2) (succ . (*2))

-- }}}

-- Succ {{{

class (Nat0 x, Nat y) => Succ x y | x -> y, y -> x where
  type NSucc x :: BN
instance Succ B0 B1 where
  type NSucc B0 = B1
instance Succ B1 (B1 :& B0) where
  type NSucc B1 = B1 :& B0
instance Nat x => Succ (x :& B0) (x :& B1) where
  type NSucc (x :& B0) = x :& B1
instance (Nat x, Succ x (y :& d)) => Succ (x :& B1) (y :& d :& B0) where
  type NSucc (x :& B1) = NSucc x :& B0

suc :: Succ x y => p x -> Proxy y
suc _ = Proxy

prd :: Succ x y => p y -> Proxy x
prd _ = Proxy

-- }}}

-- Add {{{

class (Nat0 x, Nat0 y, Nat0 z) => Add2 x y z | x y -> z, z x -> y where
instance Nat0 y => Add2 B0 y y
instance Succ y z => Add2 B1 y z
instance (Nat x, Nat (z :& dz), Add2 x y' z, AddC y' dz y)
  => Add2 (x :& B0) y (z :& dz)
instance (Nat x, Nat0 z, Add2 x y' zh, AddC y' dy y, Succ (zh :& dy) z)
  => Add2 (x :& B1) y z

-- NB: This is a non-structural constructor/deconstructor class
-- for arbitrary number plus digit
-- 2x_h + d = x
class (Nat0 xh, Nat0 x) => AddC xh (d :: BN) x | xh d -> x, x -> xh d
-- 20 + 0 = 0
instance AddC B0 B0 B0
-- 20 + 1 = 1
instance AddC B0 B1 B1
-- 2*1 + d = (2 + d) |  d <- {B0, B1}
instance (Nat0 (B1 :& d)) => AddC B1 d (B1 :& d)
-- 2(2x + d) + d' = 2 (2x + d) + d' | d, d' <- {B0, B1}
instance (Nat0 (x :& d), Nat0 (x :& d :& d')) => AddC (x :& d) d' (x :& d :& d')

class (Add2 x y z, Add2 y x z) => Add x y z | x y -> z, x z -> y, y z -> x
instance (Add2 x y z, Add2 y x z) => Add x y z

add :: Add x y z => p x -> p y -> Proxy z
add _ _ = Proxy

sub :: Add x y z => p z -> p x -> Proxy y
sub _ _ = Proxy

-- }}}

-- Equality and Order {{{

data Order
  = BLT
  | BEQ
  | BGT
  deriving (Eq,Ord,Show)

class (Nat0 x, Nat0 y) => NCompare x y (r :: Order) | x y -> r
instance NCompare B0 B0 BEQ
instance NCompare B0 B1 BLT
instance Nat (y :& dy) => NCompare B0 (y :& dy) BLT
instance NCompare B1 B0 BGT
instance NCompare B1 B1 BEQ
instance Nat (y :& dy) => NCompare B1 (y :& dy) BLT
instance Nat (x :& dx) => NCompare (x :& dx) B0 BGT
instance Nat (x :& dx) => NCompare (x :& dx) B1 BGT
instance ( Nat (x :& dx), Nat (y :& dy), NCompare dx dy dr, NCompare x y r'
         , NCS r' dr r)
  => NCompare (x :& dx) (y :& dy) r

class NCS (r1 :: Order) (r2 :: Order) (r3 :: Order) | r1 r2 -> r3
instance NCS BEQ r r
instance NCS BGT r BGT
instance NCS BLT r BLT

cmp :: NCompare x y r => p x -> p y -> Proxy r
cmp _ _ = Proxy

-- }}}

-- Disequality {{{

class (Nat0 x, Nat0 y) => NLessEq x y
instance NLessEq B0 B0
instance NLessEq B0 B1
instance Nat (y :& dy) => NLessEq B0 (y :& dy)
instance NLessEq B1 B1
instance Nat (y :& dy) => NLessEq B1 (y :& dy)
instance (Nat x, Nat y, NLessEq x y) => NLessEq (x :& B0) (y :& B0)
instance (Nat x, Nat y, NLessEq x y) => NLessEq (x :& B1) (y :& B1)
instance (Nat x, Nat y, NLessEq x y) => NLessEq (x :& B0) (y :& B1)
instance (Nat x, Nat y, NCompare x y BLT) => NLessEq (x :& B1) (y :& B0)

assertLEQ :: NLessEq x y => p x -> p y -> ()
assertLEQ _ _ = ()

-- }}}

-- Max {{{

class NMax (x :: BN) (y :: BN) (b :: Order) (r :: BN) | x y b -> r
instance NMax x y BLT y
instance NMax x y BEQ y
instance NMax x y BGT x

max :: (NCompare x y b, NMax x y b r) => p x -> p y -> Proxy r
max _ _ = Proxy

-- }}}

-- Mul {{{

-- x * y = z, x > 0
class (Nat x, Nat0 y, Nat0 z) => Mul2 x y z | x y -> z, x z -> y
-- structural induction on first arg
instance Nat0 y => Mul2 B1 y y
instance (Mul2 x y zh, AddC zh B0 z) => Mul2 (x :& B0) y z
instance (Mul2F x y z, Mul2B x y z) => Mul2 (x :& B1) y z

-- (2x + 1) * y = z, x > 0 -- fundeps forward
class (Nat x, Nat0 y, Nat0 z) => Mul2F x y z | x y -> z
-- (2x + 1) * 0 = 0
instance Nat x => Mul2F x B0 B0
-- (2x + 1) * 1 = (2x + 1)
instance Nat x => Mul2F x B1 (x :& B1)

-- (2x + 1) * 2y = 2z iff (2x + 1) * y = z
instance (Mul2F x y z, Nat x, Nat y, Nat z) => Mul2F x (y :& B0) (z :& B0)
-- (2x + 1) * (2y + 1) = 2z + 1 iff (2x + 1) * y = z instance (Mul2F x y z', Add x z' z, Nat x, Nat y, Nat z)
instance (Mul2F x y z', Add x z' z, Nat x, Nat y, Nat z)
  => Mul2F x (y :& B1) (z :& B1)

-- (2x + 1) * y = z, x > 0 -- fundeps backward
class (Nat x, Nat0 y, Nat0 z) => Mul2B x y z | z x -> y
-- (2x + 1) * 0 = 0
instance Nat x => Mul2B x B0 B0
-- (2x + 1) * 2y = 2z iff (2x + 1) * y = z
instance (Mul2B x y z, Nat x, Nat y, Nat z) => Mul2B x (y :& B0) (z :& B0)
-- (2x + 1) * y_t = 2z + 1 iff x * y = z', x * z' = z, y + 1 = y_t
-- instance (AddC y B1 yt, Mul2B x y z', Add x z' z, Nat x, Nat z)
--   Q: why AddC and not Succ?
--   A: because AddC is for (number , digit), Succ is for arbitrary number
instance (AddC y B1 yt, Mul2B x y z', Add x z' z, Nat x, Nat z)
  => Mul2B x yt (z :& B1)

-- commutative multiplication
class (Mul2 x y z, Mul2 y x z) => Mul x y z | x y -> z, x z -> y, y z -> x
instance (Mul2 x y z, Mul2 y x z) => Mul x y z

mul :: Mul x y z => p x -> p y -> Proxy z
mul _ _ = Proxy

div :: Mul x y z => p z -> p x -> Proxy y
div _ _ = Proxy

-- }}}

-- Exp2 {{{

-- y = 2^x
class (Nat0 x, Nat y) => Exp2 x y | x -> y, y -> x
instance Exp2 B0 B1
-- given:
--        x_1 + 1 = 2x + d
--        2 ^ x_1 = 2y
-------------------------
--   2 ^ (2x + d) = 2(2y)
instance (Succ x1 (x :& d), Exp2 x1 (y :& B0)) => Exp2 (x :& d) (y :& B0 :& B0)

exp2 :: Exp2 x y => p x -> Proxy y
exp2 _ = Proxy

log2 :: Exp2 x y => p y -> Proxy x
log2 _ = Proxy

{-
-- XXX: revisit
class (Nat0 x, Nat y, Nat0 r) => Exp x y r | x y -> r, x r -> y
-- 0 ^ y = 0, y > 0
instance Nat y => Exp B0 y B0
-- 1 ^ y = 1, y > 0
instance Nat y => Exp B1 y B1
-- (2x + d) ^ 1 = 2x + d
instance Nat (x :& d) => Exp (x :& d) B1 (x :& d)
-- (2x + dx) ^ (2y + dy)
--   == (x_1 + 1) ^ (2y + dy)
--   == (x_1 + 1) ^ 2y * (x_1 + 1) ^ dy
-- x_1 + 1 = 2x + dx
-- x_1 ^ (2y + dy) = (2x + dx) * r
-- instance (Succ x1 (x :& dx), Exp x1 (y :& dy), Nat (x :& dx), Nat (y :& dy)) => Exp (x :& dx) (y :& dy)
-}

-- }}}

-- GCD {{{

class (Nat0 x, Nat0 y, Nat0 z) => GCD x y z | x y -> z
instance Nat0 y => GCD B0 y y
instance Nat0 y => GCD B1 y B1
instance (NCompare (x :& dx) y r, GCD' r (x :& dx) y z) => GCD (x :& dx) y z

class (Nat x, Nat0 y, Nat0 z) => GCD' (r :: Order) x y z | r x y -> z
instance Nat x => GCD' BEQ x x x
instance (GCD y x z, Nat x) => GCD' BGT x y z
instance (Add x y1 y, GCD y1 x z, Nat x) => GCD' BLT x y z

gcd :: GCD x y z => p x -> p y -> Proxy z
gcd _ _ = Proxy

-- }}}

-- Fact {{{

class (Nat0 x, Nat y) => Fact x y | x -> y

instance Fact B0 B1
instance Fact B1 B1
instance
  ( Nat0 (x :& B0), Nat (y :& B0)
  , Succ x1 (x :& B0)
  , Fact x1 y'
  , Mul x y' y
  ) => Fact (x :& B0) (y :& B0)
-- fact 2x = y
-- if x1 + 1 = 2x
-- fact (x1 + 1) = y
-- fact x1 * 2x = y
-- y => 2y
-- fact x1 * 2x = 2y

instance
  ( Nat0 (x :& B1), Nat (y :& B0)
  , Succ x1 (x :& B0)
  , Fact x1 y'
  , Mul (x :& B0) y' z
  , Mul (x :& B1) z (y :& B0)
  ) => Fact (x :& B1) (y :& B0)
-- fact (2x + 1) = y
-- fact 2x * (2x + 1) = y
-- if x1 + 1 = 2x
-- fact (x1 + 1) * (2x + 1) = y
-- fact x1 * 2x * (2x + 1) = y
-- y => 2y
-- fact x1 * 2x * (2x + 1) = 2y
-- let z = fact x1 * 2x
-- z * (2x + 1) = 2y

fact :: Fact x y => p x -> Proxy y
fact _ = Proxy

-- }}}

-- Combine {{{

class (Nat0 n, Nat0 k, Nat0 r) => Combine n k r | n k -> r

instance Combine B0 B0 B1
instance Combine B1 B0 B1
instance Nat0 n => Combine n B1 n
instance Nat0 (k :& d) => Combine B0 (k :& d) B0
instance Nat0 (k :& d) => Combine B1 (k :& d) B0
instance
  ( Nat0 k
  , Succ n1 (n :& nd)
  , Succ k1 (k :& kd)
  , Combine n1 k1 r1
  , Combine n1 (k :& kd) r2
  , Add r1 r2 r
  ) => Combine (n :& nd) (k :& kd) r

choose :: Combine n k r => p n -> p k -> Proxy r
choose _ _ = Proxy

-- }}}

