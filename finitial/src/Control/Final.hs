{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Control.Final where

import Control.Applicative
import Prelude hiding ((+),(-),(*),(==))
import qualified Prelude
import GHC.Exts (Constraint,IsString(..))
import Data.List (intersperse)
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Proxy
import Data.Functor.Identity

-- Initial Class {{{

class Quant c (I c) => Initial (c :: (k -> *) -> Constraint) where
  data I c :: (k -> *) -> k -> *
  interp   :: c r
           => I c r a   -> r a
  lift     :: r a       -> I c r a
  traverseOpen :: Applicative f
    => Trav f      r       s
    -> Trav f (I c r) (I c s)

-- }}}

-- Initial Ops {{{

mapOpen  :: Initial c => (r :-> s) -> I c r a -> I c s a
mapOpen f = runIdentity . traverseOpen (Identity . f)

initial :: Initial c => I c r a -> I c r a
initial = id

joinOpen :: (Initial c, Initial d, d r) => I c (I d r) a -> I c r a
joinOpen = mapOpen interp

joinClosed :: forall c r a. Initial c => I c (I c r) a -> I c r a
joinClosed = interp \\ (inst :: Dict (c (I c r)))

lifted_ :: Initial c => (a -> r b) -> a -> I c r b
lifted_ = (lift .)

lifted :: (Initial c, Initial d, Initial e, d r)
  => (I c      r  a ->     s b)
  ->  I c (I d r) a -> I e s b
lifted f a = lift $ f (joinOpen a)

lifted2 :: (Initial c, Initial d, Initial e, d r, d s)
  => (I c      r  x -> I c      s  y ->     t z)
  ->  I c (I d r) x -> I c (I d s) y -> I e t z
lifted2 f a b = lift $ f (joinOpen a) (joinOpen b)

lifted3 :: (Initial c, Initial d, Initial e, d r, d s, d t)
  => (I c      r  w -> I c      s  x -> I c      t  y ->     u z)
  ->  I c (I d r) w -> I c (I d s) x -> I c (I d t) y -> I e u z
lifted3 f a b c = lift $ f (joinOpen a) (joinOpen b) (joinOpen c)

-- }}}

-- Final {{{

type IF c r = (Initial c, Final r, c r)

class Final (r ::  k -> *) where
  type Result r (a :: k) :: *
  final  :: (Initial c, c r) => proxy c -> r a -> Result r a

runFinal :: IF c r => proxy c -> I c r a -> Result r a
runFinal p = final p . interp

instance (Initial c, c r) => Final (I c r) where
  type Result (I c r) a = r a
  final _ = interp

-- }}}

-- Arith 

class Arith r where
  int  :: Int                                          -> r Int
  (+)  :: I Arith r Int  -> I Arith r Int              -> r Int
  (-)  :: I Arith r Int  -> I Arith r Int              -> r Int
  (*)  :: I Arith r Int  -> I Arith r Int              -> r Int
  (==) :: Eq a
       => I Arith r a    -> I Arith r a                -> r Bool
  if_  :: I Arith r Bool -> I Arith r a -> I Arith r a -> r a
  infixl 6 +, -
  infixl 7 *
  infix  4 ==

arith :: (Final r, Arith r) => I Arith r a -> Result r a
arith = runFinal (Proxy :: Proxy Arith)

-- Class, Initial Defs {{{

instance Initial Arith where
  data I Arith r a where
    Int  :: Int            -> I Arith r Int
    Add  :: I Arith r Int  -> I Arith r Int -> I Arith r Int
    Sub  :: I Arith r Int  -> I Arith r Int -> I Arith r Int
    Mul  :: I Arith r Int  -> I Arith r Int -> I Arith r Int
    Eql  :: Eq a
         => I Arith r a    -> I Arith r a   -> I Arith r Bool
    If   :: I Arith r Bool -> I Arith r a   -> I Arith r a    -> I Arith r a
    ALft ::         r a    -> I Arith r a
  lift = ALft
  interp t = case t of
    Int  i     -> int i
    Add  a b   -> a +  b
    Sub  a b   -> a -  b
    Mul  a b   -> a *  b
    Eql  a b   -> a == b
    If   t c a -> if_ t c a
    ALft x     -> x
  traverseOpen (f :: Trav f r s) = go
    where
    go :: Trav f (I Arith r) (I Arith s)
    go t = case t of
      Int  i     -> pure  $ Int i
      Add  a b   -> Add  <$> go a <*> go b
      Sub  a b   -> Sub  <$> go a <*> go b
      Mul  a b   -> Mul  <$> go a <*> go b
      Eql  a b   -> Eql  <$> go a <*> go b
      If   t c a -> If   <$> go t <*> go c <*> go a
      ALft x     -> ALft <$> f  x

instance Arith (I Arith r) where
  int  = Int
  (+)  = Add <:  interp
  (-)  = Sub <:  interp
  (*)  = Mul <:  interp
  (==) = Eql <:  interp
  if_  = If  <:. interp

instance Quant Arith (I Arith) where
  inst = Dict

class Arith' (p :: Bool) (r :: * -> *) where
  arithDict :: Dict (Arith r)

class IsI (c :: (k -> *) -> Constraint) (r :: k -> *) (p :: Bool) | c r -> p

instance                IsI c (I c r) True
instance (False ~ p) => IsI c      r  p

-- }}}

-- Initial instances {{{

instance Quant Show r => Show (I Arith r a) where
  showsPrec d t = parens d 0 $ case t of
    Int  i          -> [ "Int"  , S_ i               ]
    Add  a b        -> [ "Add"  , S_ a , S_ b        ]
    Sub  a b        -> [ "Sub"  , S_ a , S_ b        ]
    Mul  a b        -> [ "Mul"  , S_ a , S_ b        ]
    Eql  a b        -> [ "Eql"  , S_ a , S_ b        ]
    If   t c a      -> [ "If"   , S_ t , S_ c , S_ a ]
    ALft (x :: r a) -> [ "ALft"
                       , S_ x \\ (inst :: Dict (Show (r a)))
                       ]

instance Quant Show r => Quant Show (I Arith r) where
  inst = Dict

-- }}}

-- Render {{{

instance Arith S where
  int i     = S $ \_ -> shows i
  a +  b    = S $ \d -> parens d 6  [ S_ a , "+"  , S_ b ]
  a -  b    = S $ \d -> parens d 6  [ S_ a , "-"  , S_ b ]
  a *  b    = S $ \d -> parens d 7  [ S_ a , "*"  , S_ b ]
  a == b    = S $ \d -> parens d 4  [ S_ a , "==" , S_ b ]
  if_ t c a = S $ \d -> parens d 10 
    ["if",S_ t,"then",S_ c,"else",S_ a]

instance Final S where
  type Result S a = String
  final _ = render

-- }}}

-- Eval {{{

instance Arith E where
  int       = E
  a +  b    = E $ arith a Prelude.+  arith b
  a -  b    = E $ arith a Prelude.-  arith b
  a *  b    = E $ arith a Prelude.*  arith b
  a == b    = E $ arith a Prelude.== arith b
  if_ t c a = E $ if arith t then arith c else arith a

instance Final E where
  type Result E a = a
  final _ = eval

-- }}}

-- Extended Arith 

-- Class Def {{{

class ArithNeg r where
  neg :: I ArithNeg r Int -> r Int

instance Initial ArithNeg where
  data I ArithNeg r a where
    Neg  :: I ArithNeg r Int -> I ArithNeg r Int
    NLft ::            r a   -> I ArithNeg r a
  lift = NLft
  interp t = case t of
    Neg  a -> neg a
    NLft x -> x
  traverseOpen (f :: Trav f r s) = go
    where
    go :: Trav f (I ArithNeg r) (I ArithNeg s)
    go t = case t of
      Neg  a -> Neg  <$> go a
      NLft x -> NLft <$> f  x

instance ArithNeg (I ArithNeg r) where
  neg = Neg . interp

instance Quant ArithNeg (I ArithNeg) where
  inst = Dict

arithNeg :: (Final r, ArithNeg r) => I ArithNeg r a -> Result r a
arithNeg = runFinal (Proxy :: Proxy ArithNeg)

{-
instance (Arith r, ArithNeg r) => Arith (I ArithNeg r) where
  int  = lifted_ int
  (+)  = lifted2 (+)
  (-)  = lifted2 (-)
  (*)  = lifted2 (*)
  (==) = lifted2 (==)
  if_  = lifted3 if_
-}

instance (Arith r, ArithNeg r) => ArithNeg (I Arith r) where
  neg = lifted neg

-- }}}

-- Initial instances {{{

instance Quant Show r => Show (I ArithNeg r a) where
  showsPrec d t = case t of
    Neg  a          -> parens d 10 [ "Neg" , S_ a ]
    NLft (x :: r a) -> parens d 10
      [ "NLft"
      , S_ x \\ (inst :: Dict (Show (r a)))
      ]

instance Quant Show r => Quant Show (I ArithNeg r) where
  inst = Dict

-- }}}

-- Render {{{

instance ArithNeg S where
  neg a = S $ \d -> parens d 10 [ "negate" , S_ a ]

-- }}}

-- Eval {{{

instance ArithNeg E where
  neg a = E $ negate $ arithNeg a

-- }}}



-- Examples {{{

type DefA a  = forall r. Arith r => r a
type DefAN a = forall r. (Arith r, ArithNeg r) => r a

e0 :: DefA Int
e0 = int 4

e1 :: DefA Int
e1 = e0 + int 3

e2 :: DefA Bool
e2 = e1 == int 7

e3 :: DefA Int
e3 = if_
  (int 3 == int 1 + int 2)
  (int 4)
  (int 5)

{-
e4 :: DefAN Int
e4 = neg (int 5)
-}

-- }}}



-- Render {{{

newtype S (a :: *) = S
  { sPrec :: Int -> ShowS
  }

render :: S a -> String
render t = sPrec t 0 ""

instance Show (S a) where
  showsPrec d (S f) = f d

instance Quant Show S where
  inst = Dict

-- }}}

-- Eval {{{

newtype E a = E
  { eval :: a
  }

instance Show a => Show (E a) where
  showsPrec d (E a) = parens d 10 ["E",S_ a]

-- }}}



-- Util {{{

data Dict (c :: Constraint) where
  Dict :: c => Dict c

(\\) :: (c => r) -> Dict c -> r
r \\ Dict = r
infixl 4 \\

class Quant (c :: l -> Constraint) (f :: k -> l) where
  inst :: Dict (c (f x))

class Quant2 (c :: m -> Constraint) (f :: k -> l -> m) where
  inst2 :: Dict (c (f x y))

class Quant3 (c :: n -> Constraint) (f :: k -> l -> m -> n) where
  inst3 :: Dict (c (f x y z))

type f   :-> g  = forall x. f x   -> g x
type f  ::-> g  = forall x. f x  :-> g x
type f :::-> g  = forall x. f x ::-> g x
type Trav f r s = forall x. r x   -> f (s x)

(&) :: (a -> b -> c) -> b -> a -> c
(&) = flip
infixl 1 &

(.$) :: (a -> b) -> a -> b
f .$ x = f x
infixl 2 .$

(<:) :: (g a -> g b -> r) -> (f :-> g) -> f a -> f b -> r
(f <: g) a b = f (g a) (g b)
infixl 3 <:

(<:.) :: (g a -> g b -> g c -> r) -> (f :-> g) -> f a -> f b -> f c -> r
(f <:. g) a b c = f (g a) (g b) (g c)
infixl 3 <:.

parens :: Int -> Int -> [LS] -> ShowS
parens d dt =
    showParen (d > dt)
  . foldr (.) id
  . intersperse (showChar ' ')
  . map go
  where
  go ls = case ls of
    S_ a -> showsPrec (succ dt) a
    L_ s -> showString s

data LS
  = forall a. Show a => S_ a
  | L_ String

instance IsString LS where
  fromString = L_

-- }}}

