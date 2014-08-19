{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Nat.Unary where

import Data.Proxy

data UN
  = Z
  | S UN
  deriving (Eq,Show)

type Zero = Z
type One = S Z
type Two = S One
type Three = S Two
type Four = S Three
type Five = S Four
type Six  = S Five

zeroP  :: Proxy Z
zeroP  = Proxy
oneP   :: Proxy One
oneP   = Proxy
twoP   :: Proxy Two
twoP   = Proxy
threeP :: Proxy Three
threeP = Proxy
fourP  :: Proxy Four
fourP  = Proxy
fiveP  :: Proxy Five
fiveP  = Proxy
sixP   :: Proxy Six
sixP   = Proxy

-- WellFormed {{{

class Nat0 (n :: UN) where
  nCata :: a -> (a -> a) -> Proxy n -> a
  units :: f Z a -> (forall m. Nat0 m => f m a -> f (S m) a) -> f n a
class Nat0 n => Nat n

instance Nat0 Z where
  nCata z _ _ = z
  units z _ = z
instance Nat0 n => Nat0 (S n) where
  nCata z f = f . nCata z f . prd
  units z f = f $ units z f

instance Nat0 n => Nat (S n)

nat :: (Nat0 n, Integral l) => Proxy n -> l
nat = nCata 0 succ

-- }}}

class (Nat0 x, Nat y) => Succ x y | x -> y, y -> x
instance Nat0 x => Succ x (S x)

-- Add {{{

suc :: p n -> Proxy (S n)
suc _ = Proxy

prd :: p (S n) -> Proxy n
prd _ = Proxy

class (Nat0 m, Nat0 n, Nat0 r, r ~ NSum m n, Nat0 (NSum m n)) => Add2 m n r | m n -> r, m r -> n where
  type NSum m n :: UN
instance Nat0 n => Add2 Z n n where
  type NSum Z n = n
instance Add2 m n r => Add2 (S m) n (S r) where
  type NSum (S m) n = S (NSum m n)

class (Add2 m n r, Add2 n m r, NSum m n ~ NSum n m) => Add m n r | m n -> r, m r -> n, n r -> m

add :: Add a b c => p a -> p b -> Proxy c
add _ _ = Proxy

sub :: Add a b c => p c -> p a -> Proxy b
sub _ _ = Proxy

class (Nat0 x, Nat0 y) => NLE x y
instance Add2 x a y => NLE x y
class (Nat0 x, Nat0 y) => NLT x y
instance Add2 x (S a) y => NLT x y

-- }}}

-- Registry {{{

class (Nat0 a, Nat0 b, Nat0 c) => Registry clas a b c | clas a b -> c
class (Nat0 init, Nat0 limit, Nat0 a, Nat0 b, Nat0 c)
  => Inv clas init limit a b c | clas init a b -> c
instance
  ( Nat0 c, NLE x limit, Registry clas x b a', Add2 a' r a
  , Inv' r clas x limit a b c
  ) => Inv clas x limit a b c
class (Nat0 r, Nat0 x, Nat0 limit, Nat0 a, Nat0 b, Nat0 c)
  => Inv' r clas x limit a b c | r clas x a b -> c
instance (Nat0 a, Nat0 b, Nat0 limit, Nat0 x) => Inv' Z clas x limit a b x
instance (Nat0 x, Nat0 r, Inv clas (S x) limit a b c)
  => Inv' (S r) clas x limit a b c

-- }}}

-- Mul Div {{{

class (Nat0 a, Nat0 b, Nat0 c) => Mul a b c | a b -> c where
  type NProd a b :: UN
instance Nat0 b => Mul Z b Z where
  type NProd Z b = Z
instance (Mul a b c', Add c' b c) => Mul (S a) b c where
  type NProd (S a) b = NSum b (NProd a b)

data RegMul
instance Mul a b c => Registry RegMul a b c
class (Nat0 m, Nat n, Nat0 q, m ~ NProd n q) => Div m n q | m n -> q where
instance (Nat n, Inv RegMul Z m m n q, m ~ NProd n q) => Div m n q where

pdiv :: Div a b c => p a -> p b -> Proxy c
pdiv _ _ = Proxy

class (Nat0 a, Nat0 b, Nat0 c, c ~ NProd a b) =>  Product a b c | a b -> c, a c -> b, b c -> a
instance (Mul a b c, Div c b a, Div c a b, c ~ NProd a b) => Product a b c

prod :: Product a b c => p a -> p b -> Proxy c
prod _ _ = Proxy

-- }}}

-- Fact Combination {{{

class (Nat0 a, Nat0 b) => FactF a b | a -> b where
  type NFact a :: UN
instance FactF Z (S Z) where
  type NFact Z = S Z
instance (FactF a b', Mul (S a) b' b) => FactF (S a) b where
  type NFact (S a) = NProd (S a) (NFact a)

data RegFact
instance (Nat0 b, FactF a c) => Registry RegFact a b c

class (Nat0 a, Nat0 c) => FactR a c | a -> c
instance Inv RegFact (S Z) a a Z c => FactR a c

class (Nat0 a, Nat b) => Fact a b | a -> b, b -> a
instance (Nat b, FactF a b, FactR b a) => Fact a b

fact :: Fact a b => p a -> Proxy b
fact _ = Proxy

class (Nat0 n, Nat0 k, Nat0 r) => Combination n k r | n k -> r where
  type NChoose n k :: UN
instance Combination Z Z (S Z) where
  type NChoose Z Z = S Z
instance Nat0 k => Combination Z (S k) Z where
  type NChoose Z (S k) = Z
instance Nat0 n => Combination (S n) Z (S Z) where
  type NChoose (S n) Z = S Z
instance (Combination n k r1, Combination n (S k) r2, Add r1 r2 r) => Combination (S n) (S k) r where
  type NChoose (S n) (S k) = NSum (NChoose n k) (NChoose n (S k))

choose :: Combination n k r => p n -> p k -> Proxy r
choose _ _ = Proxy

-- }}}

