{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module Data.Staged.Class where

import Prelude hiding (Ord(..),Num(..))
import Prelude (Ord,Num)
import qualified Prelude
import GHC.Exts (Constraint)

type F = FunSym
class FunSym (r :: * -> *) where
  ifte :: r Bool -> r a -> r a -> r a
  lam  :: (r a  -> r b) ->  r (a -> b)
  app  ::  r (a -> b)   -> (r a  -> r b)
  fix  :: (r a  -> r a) ->  r a

type E = EqSym
class EqSym (r :: * -> *) where
  (==) :: Eq a => r a -> r a -> r Bool
  (/=) :: Eq a => r a -> r a -> r Bool
  infix 4 ==
  infix 4 /=

type O = OrdSym
class EqSym r => OrdSym (r :: * -> *) where
  (<)  :: Ord a => r a -> r a -> r Bool
  (>)  :: Ord a => r a -> r a -> r Bool
  (<=) :: Ord a => r a -> r a -> r Bool
  (>=) :: Ord a => r a -> r a -> r Bool
  infix 4 <
  infix 4 >
  infix 4 <=
  infix 4 >=

type N = NumSym
class NumSym (r :: * -> *) where
  fromInteger :: Num a => Integer    -> r a
  (+)         :: Num a => r a -> r a -> r a
  (-)         :: Num a => r a -> r a -> r a
  (*)         :: Num a => r a -> r a -> r a
  negate      :: Num a => r a -> r a
  infixl 6 +
  infixl 6 -
  infixl 7 *

type B = BoolSym
class BoolSym (r :: * -> *) where
  bool :: Bool   -> r Bool
  (&&) :: r Bool -> r Bool -> r Bool
  (||) :: r Bool -> r Bool -> r Bool
  not  :: r Bool -> r Bool
  xor  :: r Bool -> r Bool -> r Bool
  infixr 3 &&
  infixr 2 ||

type family Syms (cs :: [k -> Constraint]) (a :: k) :: Constraint where
  Syms '[] a       = ()
  Syms (c ': cs) a = (c a, Syms cs a)

type Sym r = Syms '[F,E,O,B,N] r

-- Syntax {{{

ifThenElse :: FunSym r => r Bool -> r a -> r a -> r a
ifThenElse = ifte

(#) :: FunSym r => r (a -> b) -> r a -> r b
(#) = app
infixl 9 #

true :: BoolSym r => r Bool
true = bool True

false :: BoolSym r => r Bool
false = bool False

-- }}}

class Phantom (r :: * -> *) where
  retype :: r a -> r b

-- Examples {{{

fact :: Sym r => r (Integer -> Integer)
fact = fix $ \f -> lam $ \x ->
  if (x <= 0)
    then 1
    else x * f # (x - 1)

powFix :: Sym r => r (Int -> Int -> Int)
powFix = lam $ \x -> fix $ \f -> lam $ \n ->
  if (n <= 0)
    then 1
    else x * (f # n - 1)

{-
pow7 :: Sym '[F,O,N] r => r (Int -> Int)
pow7 = lam $ \x -> powFix # x # 7

pow7_2 :: Sym '[F,O,N] r => r Int
pow7_2 = pow7 # 2

-- }}}
-}

