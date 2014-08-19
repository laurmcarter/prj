{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Diagrams.Simplex where

import Control.Lens
import Data.Proxy
import Data.Void

import Diagrams.Coordinates
import Diagrams.TwoD.Types (R2)
import Diagrams.ThreeD.Types (R3)

-- Uniform {{{

class Uniform m a where
  uniform :: Proxy a -> m -> m

instance Uniform () a where
  uniform _ _ = ()

instance (Uniform m a) => Uniform (m :& a) a where
  uniform p (m :& a) = uniform p m :& a

-- }}}

-- Mapping {{{

class (Uniform m a, Uniform (Of m b) b) => Mapping m a b | m -> a where
  type Of m b :: *
  mapN :: Proxy a -> Proxy b -> (a -> b) -> m -> Of m b

instance Mapping () a b where
  type Of () b = ()
  mapN _ _ _ _ = ()

instance (Mapping m a b) => Mapping (m :& a) a b where
  type Of (m :& a) b = Of m b :& b
  mapN pa pb f (m :& a) = mapN pa pb f m :& f a

-- }}}

-- Adding {{{

-- Join two (uniform) collections into one (uniform) collection
class (Uniform m a, Uniform n a, Uniform (Add m n) a) => Adding m n a | m -> a, n -> a where
  type Add m n :: *
  add :: m -> n -> Add m n

instance (Uniform m a) => Adding m () a where
  type Add m () = m
  add m _ = uniform (Proxy :: Proxy a) m

instance (Adding m n a) => Adding m (n :& a) a where
  type Add m (n :& a) = Add m n :& a
  add m (n :& a) = add m n :& a

-- }}}

-- Subtracting {{{

class (Uniform m a, Uniform (Sub m n) a) => Subtracting m n a | m -> a where
  type Sub m n :: *
  sub :: m -> n -> Sub m n

instance Subtracting () () a where
  type Sub () () = ()
  sub _ _ = ()

instance (Uniform m a) => Subtracting (m :& a) () a where
  type Sub (m :& a) () = m :& a
  sub (m :& a) _ = uniform (Proxy :: Proxy a) m :& a

instance (Uniform m a, Subtracting m n a) => Subtracting (m :& a) (n :& b) a where
  type Sub (m :& a) (n :& b) = Sub m n
  sub (m :& _) (n :& _) = sub m n

-- }}}

-- Choosing {{{

class
  ( Uniform n a
  , Uniform k b
  , Uniform (Choose n k) (Of k a)
  ) => Choosing n k a b | n -> a, k -> b where
  type Choose n k :: *
  choose :: Proxy k -> n -> Choose n k

instance Choosing () () a b where
  type Choose () () = () :& ()
  choose _ _ = () :& ()

instance (Uniform k b) => Choosing () (k :& b) a b where
  type Choose () (k :& b) = ()
  choose _ _ = ()

instance (Uniform n a) => Choosing (n :& a) () a b where
  type Choose (n :& a) () = () :& ()
  choose _ _ = () :& ()

instance
  ( Uniform n a
  , Uniform k b
  , Choosing n k a b
  , Choosing n (k :& b) a b
  , Mapping (Choose n k) (Of k a) (Of k a :& a) 
  , Adding (Of (Choose n k) (Of k a :& a)) (Choose n (k :& b)) (Of k a :& a)
  ) => Choosing (n :& a) (k :& b) a b where
  type Choose (n :& a) (k :& b) = Add (Of (Choose n k) (Of k a :& a)) (Choose n (k :& b))
  choose prkb (n :& a) = add
    ( mapN prka prka' (:& a)
      $ choose prk n
    ) $ choose prkb n
    where
    prk   = Proxy :: Proxy k
    prka  = Proxy :: Proxy (Of k a)
    prka' = Proxy :: Proxy (Of k a :& a)

-- }}}

type R0 = ()
type R1 = Double

-- Dimensional {{{

class Dimensional r n | r -> n, n -> r where
  type LowerDim r :: *
  type LowerDim r = PrevDim r
  fromList :: [a] -> Maybe (Of n a)

instance Dimensional R0 () where
  type LowerDim R0 = Void
  fromList as = case as of
    [] -> Just ()
    _  -> Nothing

instance Dimensional R1 (() :& a) where
  type LowerDim R1 = R0
  fromList as = case as of
    [a] -> Just $ () :& a
    _   -> Nothing

instance Dimensional R2 (() :& a :& a) where
  fromList as = case as of
    [a,b] -> Just $ () :& a :& b
    _     -> Nothing

instance Dimensional R3 (() :& a :& a :& a) where
  fromList as = case as of
    [a,b,c] -> Just $ () :& a :& b :& c
    _       -> Nothing

r0 :: Proxy R0
r0 = Proxy
r1 :: Proxy R1
r1 = Proxy
r2 :: Proxy R2
r2 = Proxy
r3 :: Proxy R3
r3 = Proxy

-- }}}

-- Simplicial {{{

type OfPlus1 n a = Of n a :& a
type Plus1 n a = n :& a

class Dimensional r n => Simplicial r n where
  data Simplex r :: * -> *
  type SimplexDecomp r b :: *
  type SimplexDecomp r b = (Simplex (LowerDim r) b,b)
  _Simplex :: Iso (OfPlus1 n b) (OfPlus1 n c) (Simplex r b) (Simplex r c)
  _PrevSimplex :: Iso (Simplex r b) (Simplex r c) (SimplexDecomp r b) (SimplexDecomp r c)

instance Simplicial R0 () where
  data Simplex R0 a = R0Simplex !a
  type SimplexDecomp R0 a = Void
  _Simplex = iso toSimplex fromSimplex
    where
    toSimplex (_ :& a) = R0Simplex a
    fromSimplex (R0Simplex a) = () :& a
  _PrevSimplex = error "No Simplex of negative dimension"

instance Simplicial R1 (() :& a) where
  data Simplex R1 a = R1Simplex !a !a
  _Simplex = iso toSimplex fromSimplex 
    where
    toSimplex (_ :& a :& b) = R1Simplex a b
    fromSimplex (R1Simplex a b) = () :& a :& b
  _PrevSimplex = iso toPrev fromPrev
    where
    toPrev (R1Simplex a b) = (R0Simplex a,b)
    fromPrev (R0Simplex a,b) = R1Simplex a b

instance Simplicial R2 (() :& a :& a) where
  data Simplex R2 a = R2Simplex !a !a !a
  _Simplex = iso toSimplex fromSimplex
    where
    toSimplex (_ :& a :& b :& c) = R2Simplex a b c
    fromSimplex (R2Simplex a b c) = () :& a :& b :& c

instance Simplicial R3 (() :& a :& a :& a) where
  data Simplex R3 a = R3Simplex !a !a !a !a
  _Simplex  = iso toSimplex fromSimplex
    where
    toSimplex (_ :& a :& b :& c :& d) = R3Simplex a b c d
    fromSimplex (R3Simplex a b c d) = () :& a :& b :& c :& d

toVertices :: Simplicial r n => Simplex r a -> OfPlus1 n a
toVertices = view $ from _Simplex

fromVertices :: Simplicial r n => OfPlus1 n a -> Simplex r a
fromVertices = view _Simplex

simplex :: Simplicial r n => a -> Of n a -> Simplex r a
simplex p s = fromVertices $ s :& p

-- }}}

-- Faces {{{

{-
type LowerSimplex r k = Simplex (DimSubK r k)

type SimplexFace r k a = Simplex (DimSubK r k) a

class (Simplicial r) => Faces r k where
  faces :: Proxy k -> Simplex r a -> Choose (NPlus1 r a) (NSubKPlus1 r k a) (LowerSimplex r k a)

instance Faces R0 () where
  faces _ s = () :& s


instance Faces R1 () where
  faces _ s = () :& s
instance Faces R1 (() :& a) where
  faces _ s = mapDim r2 R0Simplex $ toVertices s


instance Faces R2 () where
  faces _ s = () :& s
instance Faces R2 (() :& a) where
  faces _ s = undefined
    where
    map2 = undefined -- mapDim r2 (fromVertices . (:& c)) :: N R2 (N R1 a) -> N R2 (Simplex R1 a)
    ab :& c = toVertices s

instance Faces R3 () where
  faces _ s = () :& s
-}

-- }}}

