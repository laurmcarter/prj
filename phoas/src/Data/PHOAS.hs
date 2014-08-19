{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.PHOAS where

import GHC.Exts (Constraint,Any)

import Control.Applicative
import Control.Monad

import Data.Monoid (Monoid(..),(<>))
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Bifunctor (Bifunctor)
import qualified Data.Bifunctor as B
import Data.Profunctor (Profunctor)
import qualified Data.Profunctor as P

-- Nat {{{

data N
  = Z
  | S N
  deriving (Eq,Ord,Show)

data Nat (n :: N) where
  Z_ :: Nat Z
  S_ :: Nat n -> Nat (S n)

deriving instance Show (Nat n)

class KnownC n => Known (n :: N) where
  type KnownC n :: Constraint
  nat :: Nat n

instance Known Z where
  type KnownC Z = (() :: Constraint)
  nat = Z_

instance Known n => Known (S n) where
  type KnownC (S n) = Known n
  nat = S_ nat

natP :: Nat (S x) -> Nat x
natP (S_ x) = x

data Le :: N -> N -> * where
  LeZ :: Known y  => Le  Z    (S y)
  LeS :: (x <= y) => Le (S x) (S y)

deriving instance Show (Le x y)

class LeC x y => (x :: N) <= (y :: N) where
  type LeC x y :: Constraint
  leProof :: Le x y

instance Known y => Z <= (S y) where
  type LeC Z (S y) = Known y
  leProof = LeZ

instance (x <= y) => S x <= S y where
  type LeC (S x) (S y) = x <= y
  leProof = LeS

(\\<=) :: ((x <= y) => r) -> Le x y -> r
r \\<= le = case le of
  LeZ -> r
  LeS -> r
infixl 1 \\<=

-- }}}

-- Fin {{{

data Fin (n :: N) where
  FZ :: Known n => Fin (S n)
  FS :: Fin n   -> Fin (S n)

deriving instance Show (Fin n)

fin0 :: Known x => Fin (S x)
fin0 = FZ

finP :: Fin (S (S x)) -> Fin (S x)
finP x = case x of
  FZ    -> FZ
  FS x' -> x'

weaken1 :: Fin x -> Fin (S x)
weaken1 x = case x of
  FZ    -> FZ
  FS x' -> FS $ weaken1 x'

tighten1 :: Fin (S x) -> Either (Nat x) (Fin x)
tighten1 x = case x of
  FZ    -> Left nat
  FS x' -> Right x'

-- }}}

-- V {{{

data V (n :: N) a where
  Nil  :: V Z a
  (:>) :: a -> V n a -> V (S n) a
infixr 4 :>

deriving instance Show a => Show (V n a)

instance Functor (V n) where
  fmap f v = case v of
    Nil     -> Nil
    a :> v' -> f a :> fmap f v'

replicateN :: Known x => a -> V x a
replicateN = replN nat

replN :: Nat x -> a -> V x a
replN x a = case x of
  Z_    -> Nil
  S_ x' -> a :> replN x' a

instance Known x => Applicative (V x) where
  pure    = replicateN
  fs <*> xs = case (fs,xs) of
    (Nil,Nil) -> Nil
    (f :> fs',x :> xs') -> f x :> (fs' <*> xs')
    _ -> error "unreachable"

instance Foldable (V x) where
  foldMap f v = case v of
    Nil     -> mempty
    a :> v' -> f a <> F.foldMap f v'

instance Traversable (V x) where
  traverse f v = case v of
    Nil     -> pure Nil
    a :> v' -> (:>) <$> f a <*> T.traverse f v'

vhead :: V (S n) a -> a
vhead (a :> _) = a

vtail :: V (S n) a -> V n a
vtail (_ :> v) = v

vinit :: V (S n) a -> V n a
vinit (a :> v) = vinit' a v
  where
  vinit' :: a -> V n a -> V n a
  vinit' a as = case as of
    Nil      -> Nil
    b :> as' -> a :> vinit' b as' 

vlast :: V (S n) a -> a
vlast (a :> v) = vlast' a v
  where
  vlast' :: a -> V n a -> a
  vlast' a as = case as of
    Nil      -> a
    b :> as' -> vlast' b as'

(!) :: V x a -> Fin x -> a
v ! x = case (v,x) of
  (a :> _ ,FZ   ) -> a
  (_ :> v',FS x') -> v' ! x'
  _               -> error "unreachable"

fix :: (a -> a) -> a
fix f = let x = f x in x

viterate :: Known x => (a -> a) -> a -> V x a
viterate = viter nat

viter :: Nat x -> (a -> a) -> a -> V x a
viter x s z = case x of
  Z_    -> Nil
  S_ x' -> z :> viter x' s (s z)

-- }}}

-- List {{{

data List (as :: [*]) where
  NilL :: List '[]
  (:*) :: a -> List as -> List (a ': as)
infixr 4 :*

instance Show (List '[]) where
  show NilL = "NilL"

instance (Show a, Show (List as)) => Show (List (a ': as)) where
  showsPrec d (a :* as) = showParen (d > 4)
    $ showsPrec 5 a
    . showString " :* "
    . showsPrec 4 as

type family Init (as :: [k]) :: [k] where
  Init (a ': as) = Init' a as

type family Init' (a :: k) (as :: [k]) :: [k] where
  Init' a '[]       = '[]
  Init' a (b ': as) = a ': Init' b as

type family Last (as :: [k]) :: k where
  Last (a ': as) = Last' a as

type family Last' (a :: k) (as :: [k]) :: k where
  Last' a '[]       = a
  Last' a (b ': as) = Last' b as

lhead :: List (a ': as) -> a
lhead (h :* _) = h

ltail :: List (a ': as) -> List as
ltail (_ :* t) = t

linit :: List (a ': as) -> List (Init' a as)
linit (h :* t) = linit' h t
  where
  linit' :: a -> List as -> List (Init' a as)
  linit' a as = case as of
    NilL     -> NilL
    b :* as' -> a :* linit' b as'

llast :: List (a ': as) -> Last' a as
llast (h :* t) = llast' h t
  where
  llast' :: a -> List as -> Last' a as
  llast' a as = case as of
    NilL     -> a
    b :* as' -> llast' b as'

type family Repeat (x :: N) (a :: k) :: [k] where
  Repeat  Z    a = '[]
  Repeat (S x) a = a ': Repeat x a

lrepl :: Nat x -> a -> List (Repeat x a)
lrepl x a = case x of
  Z_    -> NilL
  S_ x' -> a :* lrepl x' a

newtype ListSame (x :: N) (a :: *) = ListSame
  { listSame :: List (Repeat x a)
  }

lreplicate :: forall x a. Known x => a -> ListSame x a
lreplicate = ListSame . lrepl (nat :: Nat x)

data Mem :: k -> [k] -> * where
  Head :: Mem a (a ': as)
  Tail :: (Member a as) => Mem a (b ': as)

deriving instance Show (Mem a as)

class Member (a :: k) (as :: [k]) where
  member  :: Mem a as
  memP    :: Mem a (b ': as) -> Mem a as

instance Member a (a ': as) where
  member  = Head
  memP _  = Head

instance Member a as => Member a (b ': as) where
  member = Tail
  memP _ = Tail

data Sub :: [k] -> [k] -> * where
  Null :: Sub '[] bs
  Sub  :: (Member a bs, Subset as bs) => Sub (a ': as) bs

deriving instance Show (Sub as bs)

class SubC as bs => Subset (as :: [k]) (bs :: [k]) where
  type SubC as bs :: Constraint
  subset :: Sub as bs

instance Subset '[] bs where
  type SubC '[] bs = (() :: Constraint)
  subset = Null

instance (Member a bs, Subset as bs) => Subset (a ': as) bs where
  type SubC (a ': as) bs = (Member a bs, Subset as bs)
  subset = Sub

(\\⊆) :: (Subset as bs => r) -> Sub as bs -> r
r \\⊆ ss = case ss of
  Null -> r
  Sub  -> r

-- }}}

data Rec f a where
  Var    :: a
         -> Rec f a
  LetRec :: Nat (S n)
         -> (V (S n) a -> V (S n) (f (Rec f a)))
         -> (V (S n) a -> Rec f a)
         -> Rec f a
  In     :: f (Rec f a)
         -> Rec f a

newtype R f = R { instantiate :: forall a. Rec f a }

{-
gfold :: forall f t c. Functor f
  => (t -> c)
  -> (forall n. Known n => (V n t -> V n c) -> (V n t -> c) -> c)
  -> (f c -> c)
  -> R f -> c
gfold v l f = trans . instantiate
  where
  trans :: Rec f t -> c
  trans t = case t of
    Var x   -> v x
    Mu  g h -> _
    In  t'  -> f $ fmap trans t'
-}

{-
fold :: Functor f => (f c -> c) -> c -> R f -> c
fold alg k = gfold id (\g -> vhead $ g $ replicateN k) alg
-}

{-
cfold :: Functor f => (f t -> t) -> R f -> t
cfold = gfold id (vhead . _)
-}

newtype Id a = Id
  { runId :: a
  } deriving (Eq,Ord,Show)

data StreamF a r
  = Cons a r
  deriving (Eq,Ord,Show)

type Stream a = R (StreamF a)

pattern V1 a         = a :> Nil
pattern V2 a b       = a :> V1 b
pattern V3 a b c     = a :> V2 b c
pattern V4 a b c d   = a :> V3 b c d
pattern V5 a b c d e = a :> V4 b c d e
pattern x :>: y = x :> y :> Nil
infix 5 :>:

s0 :: Stream Int
s0 = R $ LetRec (S_ $ S_ $ S_ Z_)
   & ( \(V3 x y z) -> V3 (Cons 1 $ Var y)
                          (Cons 2 $ Var z)
                          (Cons 3 $ Var x))
   &   \(V3 x y z) -> Var x

($$) :: (a -> b) -> a -> b
f $$ x = f x
infixr 2 $$

(&) :: (a -> b) -> a -> b
f & x = f x
infixl 1 &

pattern L1    x = x :* NilL
pattern x :*: y = x :* y :* NilL
infix 5 :*:

-- Tuple {{{

class Tuple (as :: [*]) (bs :: [*]) (s :: *) (t :: *) | as -> s, bs -> t, as t -> bs, bs s -> as where
  _ls  :: (s -> t) -> List as -> List bs
  _tup :: (List as -> List bs) -> s -> t

instance Tuple '[] '[] () () where
  _ls _  = id
  _tup _ = id

instance Tuple '[a] '[b] a b where
  _ls f (L1 a) = L1 $ f a
  _ls _ _ = error "unreachable"
  _tup f a = case f (L1 a) of
    L1 b -> b
    _ -> error "unreachable"

instance Tuple '[a,b] '[c,d] (a,b) (c,d) where
  _ls f (a :*: b) = c :*: d
    where
    (c,d) = f (a,b)
  _ls _ _ = error "unreachable"
  _tup f (a,b) = case f (a :*: b) of
    c :*: d -> (c,d)
    _ -> error "unreachable"

instance Tuple '[a,b,c] '[d,e,f] (a,b,c) (d,e,f) where
  _ls fn (a :* b :*: c) = d :* e :*: f
    where
    (d,e,f) = fn (a,b,c)
  _ls _ _ = error "unreachable"
  _tup f (a,b,c) = case f (a :* b :*: c) of
    d :* e :*: f -> (d,e,f)
    _ -> error "unreachable"

instance Tuple '[a,b,c,d] '[e,f,g,h] (a,b,c,d) (e,f,g,h) where
  _ls fn (a :* b :* c :*: d) = e :* f :* g :*: h
    where
    (e,f,g,h) = fn (a,b,c,d)
  _ls _ _ = error "unreachable"
  _tup f (a,b,c,d) = case f (a :* b :* c :*: d) of
    e :* f :* g :*: h -> (e,f,g,h)
    _ -> error "unreachable"

instance Tuple '[a,b,c,d,e] '[f,g,h,i,j] (a,b,c,d,e) (f,g,h,i,j) where
  _ls fn (a :* b :* c :* d :*: e) = f :* g :* h :* i :*: j
    where
    (f,g,h,i,j) = fn (a,b,c,d,e)
  _ls _ _ = error "unreachable"
  _tup f (a,b,c,d,e) = case f (a :* b :* c :* d :*: e) of
    f :* g :* h :* i :*: j -> (f,g,h,i,j)
    _ -> error "unreachable"

-- }}}

