{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Data.Rep where

import Control.Arrow
  ( Arrow(..)
  , ArrowChoice(..)
  , ArrowZero(..)
  , ArrowPlus(..)
  , (>>>),(<<<)
  )
import Control.Category
import Prelude hiding (id,(.),sum)
import Data.Function (on)
import Data.Bifunctor
import Data.Bifoldable
import Data.Biapplicative hiding ((<<$>>))

class Generic g where
  unit :: g ()
  sum  :: g a -> g b -> g (Either a b)
  prod :: g a -> g b -> g (a,b)
  dat  :: (b <-> a)  -> g a -> g b
  char :: g Char
  int  :: g Integer

class TypeRep t where
  rep :: Generic g => g t

instance TypeRep () where
  rep = unit

instance TypeRep Char where
  rep = char

instance TypeRep Integer where
  rep = int

instance (TypeRep a, TypeRep b) => TypeRep (Either a b) where
  rep = sum rep rep

instance (TypeRep a, TypeRep b) => TypeRep (a,b) where
  rep = prod rep rep



-- <-> {{{

data a <-> b = EP
  { to   :: a -> b
  , from :: b -> a
  }
infix 5 <->

instance Category (<->) where
  id      = EP id id
  bc . ab = EP
    { to   = to bc . to ab
    , from = from ab . from bc
    }

inv :: (a <-> b) -> (b <-> a)
inv ab = EP (from ab) (to ab)

-- }}}

-- Invs {{{

enum :: Enum a => (a <-> Int)
enum = EP fromEnum toEnum

str :: (Show a, Read a) => (a <-> String)
str = EP show read

intgr :: (Num a, Integral a) => (a <-> Integer)
intgr = EP toInteger fromInteger

ratnl :: (Fractional a, Real a) => (a <-> Rational)
ratnl = EP toRational fromRational

-- }}}

newtype GEq a = GEq
  { geq :: a -> a -> Bool
  }

instance Generic GEq where
  unit     = GEq (==)
  char     = GEq (==)
  int      = GEq (==)
  dat  f a = GEq $ f ==>> uncurry (geq a)
  prod a b = GEq $ biAll .: biliftA2 (geq a) (geq b)
  sum  a b = GEq $ curry $ undefined
    where
    f = geq a
    g = geq b
    h = f <+> g
    -- \case
    -- (Left  x,Left  y) -> geq a x y
    -- (Right x,Right y) -> geq b x y
    -- _                 -> False

-- TypeRep {{{

{-
data Rep t where
  RUnit :: (t <-> ())
        -> Rep t
  RInt  :: (t <-> Integer)
        -> Rep t
  RRat  :: (t <-> Rational)
        -> Rep t
  RChar :: (t <-> Char)
        -> Rep t
  RSum  :: Rep a -> Rep b
        -> (t <-> Either a b)
        -> Rep t
  RProd :: Rep a -> Rep b
        -> (t <-> (a,b))
        -> Rep t
-}

{-
mkRep :: forall a b. (Generic g, TypeRep b) => (a <-> b) -> Rep a
mkRep f = case (rep :: Rep b) of
  RUnit g     -> RUnit     $ g . f
  RInt  g     -> RInt      $ g . f
  RRat  g     -> RRat      $ g . f
  RChar g     -> RChar     $ g . f
  RSum  a b g -> RSum  a b $ g . f
  RProd a b g -> RProd a b $ g . f
-}

{-
eq :: Rep t -> t -> t -> Bool
eq r = case r of
  RUnit f     -> f ==> (==)
  RInt  f     -> f ==> (==)
  RRat  f     -> f ==> (==)
  RChar f     -> f ==> (==)
  RSum  a b f -> f ==>> \case
    (Left  x,Left  y) -> eq a x y
    (Right x,Right y) -> eq b x y
    _                 -> False
  RProd a b f -> f ==>> \case
    ((w,y),(x,z)) -> eq a w x && eq b y z

req :: TypeRep t => t -> t -> Bool
req = eq rep
-}

{-
-- Base Instances {{{

instance TypeRep () where
  rep = RUnit id

instance TypeRep Integer where
  rep = RInt id

instance TypeRep Char where
  rep = RChar id

instance TypeRep Rational where
  rep = RRat id

instance TypeRep Int where
  rep = RInt int

instance (TypeRep a, TypeRep b) => TypeRep (a,b) where
  rep = RProd rep rep id

instance (TypeRep a, TypeRep b) => TypeRep (Either a b) where
  rep = RSum rep rep id

-- }}}

instance TypeRep a => TypeRep [a] where
  rep = mkRep $ EP
    ( \case
      []   -> Left ()
      a:as -> Right (a,as) )
    ( \case
      Left  ()     -> []
      Right (a,as) -> a:as )
-}

-- }}}

-- Combinators {{{

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 8 .:

biAll :: Bifoldable t => t Bool Bool -> Bool
biAll = biall id id

(<<$>>) :: Bifunctor p => (a -> b) -> p a a -> p b b
(<<$>>) f = bimap f f
infixl 4 <<$>>

(==>) :: (a <-> b) -> (b -> b -> c) -> a -> a -> c
(==>) f g = g `on` to f
infixr 1 ==>

(<==) :: (a <-> b) -> (a -> a -> c) -> b -> b -> c
(<==) f g = g `on` from f
infixr 1 <==

(==>>) :: (a <-> b) -> ((b,b) -> c) -> a -> a -> c
(==>>) f g = curry g `on` to f
infixr 1 ==>>

(<<==) :: (a <-> b) -> ((a,a) -> c) -> b -> b -> c
(<<==) f g = curry g `on` from f
infixr 1 <<==

-- }}}

