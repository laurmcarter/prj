{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Vec where

import Prelude hiding (head,tail,init,last,length)

import Data.Nat.Unary
import qualified Linear as Lin
import qualified Data.AdditiveGroup as VS
import qualified Data.VectorSpace as VS

import Control.Applicative hiding (empty)
import Control.Lens hiding ((<.>))
import Data.Proxy
import Data.Monoid hiding ((<>), Product(..))
import Data.Functor.Bind (Apply)
import qualified Data.Functor.Bind as ST
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Semigroup hiding (Product(..))
import Data.Semigroup.Foldable (Foldable1)
import qualified Data.Semigroup.Foldable as SF
import Data.Semigroup.Traversable (Traversable1)
import qualified Data.Semigroup.Traversable as ST

data Vec (n :: UN) a where
  Nil :: Vec Z a
  (:*) :: a -> Vec n a -> Vec (S n) a

-- Convenient syntax for ending vectors
(*:) :: a -> a -> Vec (S (S Z)) a
a *: b = a :* b :* Nil

infixr 4 :*
infixr 4 *:

asProxy :: Vec n a -> Proxy n
asProxy _ = Proxy

-- Constructive Operations {{{

empty :: Vec Z a
empty = Nil

singleton :: a -> Vec (S Z) a
singleton = (:* Nil)

fromList :: FromList n => [a] -> Maybe (Vec n a)
fromList = preview _List

toList :: FromList n => Vec n a -> [a]
toList = review _List

fromSet :: (FromSet n, Ord a) => Set a -> Maybe (Vec n a)
fromSet = preview _Set

toSet :: (FromSet n, Ord a) => Vec n a -> Set a
toSet = review _Set

class Nat0 n => FromList n where
  _List :: Prism' [a] (Vec n a)
instance FromList Z where
  _List = prism' (const []) $ \l -> case l of
    [] -> Just Nil
    _  -> Nothing
instance FromList n => FromList (S n) where
  _List = prism'
    (\(a :* v) -> a : review _List v)
    (\l -> case l of
      (a : l') -> (a :*) <$> preview _List l'
      _        -> Nothing)

class Nat0 n => FromSet n where
  _Set :: Ord a => Prism' (Set a) (Vec n a)
instance FromSet Z where
  _Set = prism' (const S.empty) $ \s -> case S.size s of
    0 -> Just Nil
    _ -> Nothing
instance FromSet n => FromSet (S n) where
  _Set = prism'
    (\(a :* v) -> S.insert a $ review _Set v)
    (\s -> case S.minView s of
      Just (a,s') -> (a :*) <$> preview _Set s'
      _ -> Nothing)

append :: Vec m a -> Vec n a -> Vec (NSum m n) a
append vec v2 = case vec of
  Nil       -> v2
  (a :* v1) -> a :* append v1 v2

flatten :: Vec m (Vec n a) -> Vec (NProd m n) a
flatten vec = case vec of
  Nil       -> Nil
  (v :* vs) -> append v $ flatten vs

class Combine k where
  combine :: Proxy k -> Vec n a -> Vec (NChoose n k) (Vec k a)
instance (Nat0 (NChoose n Z)) => Combine Z where
  combine _ v = case v of
    Nil      -> Nil :* Nil
    (_ :* _) -> Nil :* Nil
instance Combine k => Combine (S k) where
  combine p v = case v of
    Nil -> Nil
    (a :* v') -> append v1 v2
      where
      v1 = fmap (a :*) $ combine (prd p) v'
      v2 = combine p v'

-- }}}

-- Destructive Operators {{{

length :: (Nat0 n, Integral l) => Vec n a -> l
length = nat . asProxy

head :: Vec (S n) a -> a
head (a :* _) = a

tail :: Vec (S n) a -> Vec n a
tail (_ :* v) = v

init :: Vec (S n) a -> Vec n a
init (a :* v) = case v of
  Nil       -> Nil
  (_ :* _) -> a :* init v

last :: Vec (S n) a -> a
last (a :* v) = case v of
  Nil -> a
  (_ :* _) -> last v

member :: Eq a => Vec n a -> a -> Bool
member vec a = case vec of
  Nil -> False
  (b :* v)
    | a == b    -> True
    | otherwise -> member v a

-- }}}

-- Eq Ord Show {{{

instance Eq a => Eq (Vec n a) where
  Nil == Nil = True
  (a :* v1) == (b :* v2) = a == b && v1 == v2
  _ == _ = False

instance Ord a => Ord (Vec Z a) where
  compare Nil Nil = EQ
instance (Ord a, Ord (Vec n a)) => Ord (Vec (S n) a) where
  compare (a :* v1) (b :* v2) = compare a b `mappend` compare v1 v2

instance Show a => Show (Vec n a) where
  showsPrec _ Nil = showString "Nil"
  showsPrec n (a :* v) = showParen (n > up_prec) $
    showsPrec (up_prec+1) a .
    showString " :* "       .
    showsPrec (up_prec+1) v
    where
    up_prec = 5

-- }}}

-- Semigroup Monoid {{{

instance Semigroup (Vec Z a) where
  _ <> _ = Nil

instance (Semigroup a, Semigroup (Vec n a)) => Semigroup (Vec (S n) a) where
  (a :* v1) <> (b :* v2) = (a <> b) :* (v1 <> v2)

instance Monoid (Vec Z a) where
  mempty = Nil
  mappend _ _ = Nil

instance (Nat0 n, Monoid a, Monoid (Vec n a)) => Monoid (Vec (S n) a) where
  mempty = mempty :* mempty
  mappend (a :* v1) (b :* v2) = mappend a b :* mappend v1 v2

-- }}}

-- Functor Applicative Apply {{{

instance Functor (Vec n) where
  fmap f v = case v of
    Nil -> Nil
    (a :* v') -> f a :* fmap f v'

instance Nat0 n => Applicative (Vec n) where
  pure a = units Nil (a :*)
  fs <*> as = case (fs,as) of
    (Nil,Nil) -> Nil
    (f :* fs' ,a :* as') -> f a :* (fs' <*> as')

{-
instance Applicative (Vec Z) where
  pure _ = Nil
  _ <*> _ = Nil
instance Applicative (Vec n) => Applicative (Vec (S n)) where
  pure a = a :* pure a
  (f :* fs) <*> (a :* as) = f a :* (fs <*> as)
-}

instance Apply (Vec (S n)) where
  (f :* fs) <.> (a :* as) = case (fs,as) of
    (Nil,Nil) -> f a :* Nil
    (_ :* _,_ :* _) -> f a :* (fs ST.<.> as)

-- }}}

-- Foldable(1) Traversable(1) {{{

instance Foldable (Vec n) where
  foldMap f v = case v of
    Nil -> mempty
    (a :* v') -> f a `mappend` F.foldMap f v'

instance Foldable1 (Vec (S n)) where
  foldMap1 f (a :* v) = case v of
    Nil -> f a
    (_ :* _) -> f a <> SF.foldMap1 f v

instance Traversable (Vec n) where
  traverse f v = case v of
    Nil -> pure Nil
    (a :* v') -> (:*) <$> f a <*> T.traverse f v'

instance Traversable1 (Vec (S n)) where
  traverse1 f (a :* v) = case v of
    Nil -> (:* Nil) <$> f a
    (_ :* _) -> (:*) <$> f a ST.<.> ST.traverse1 f v

-- }}}

-- Linear {{{

instance Applicative (Vec n) => Lin.Additive (Vec n) where
  zero = pure 0

instance Applicative (Vec n) => Lin.Metric (Vec n) where
  x `dot` y = F.foldr (+) 0 $ liftA2 (*) x y

-- }}}

-- VectorSpace {{{

instance (Applicative (Vec n), VS.VectorSpace a) => VS.AdditiveGroup (Vec n a) where
  zeroV   = pure VS.zeroV
  (^+^)   = liftA2 (VS.^+^)
  negateV = fmap VS.negateV

instance (Applicative (Vec n), VS.VectorSpace a) => VS.VectorSpace (Vec n a) where
  type Scalar (Vec n a) = VS.Scalar a
  s *^ v = fmap (s VS.*^) v

instance (Applicative (Vec n), VS.InnerSpace a) => VS.InnerSpace (Vec n a) where
  v1 <.> v2 = VS.sumV $ liftA2 (VS.<.>) v1 v2

-- }}}

