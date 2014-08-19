{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Syntax.Equational where

import Control.Lens hiding ((|>))
import Prelude hiding (not, (&&), (||))
import Data.Type.Equality
import Data.Type.Bool

data family W (x :: k) :: *

(=<|) :: x :~: y -> y :~: z -> x :~: z
(=<|) = trans
infixr 1 =<|
{-# INLINE (=<|) #-}

(|>) :: x :~: y -> W y -> x :~: y
p |> _ = p
infixr 2 |>
{-# INLINE (|>) #-}

-- Ops {{{

type B (x :: Bool) = W x

data instance W (x :: Bool) where
  True_  :: W True
  False_ :: W False

newtype Not_ (p :: Bool -> *) (x :: Bool) = Not_
  { unNot :: p (Not x)
  }

newtype And_ (p :: Bool -> *) (x :: Bool) (y :: Bool) = And_
  { unAnd :: p (x && y)
  }

newtype Or_ (p :: Bool -> *) (x :: Bool) (y :: Bool) = Or_
  { unOr :: p (x || y)
  }

not :: B x -> B (Not x)
not x = case x of
  True_  -> False_
  False_ -> True_
{-# INLINE not #-}

(&&) :: B x -> B y -> B (x && y)
x && y = case x of
  True_  -> y
  False_ -> False_
infixr 3 &&
{-# INLINE (&&) #-}

(||) :: B x -> B y -> B (x || y)
x || y = case x of
  True_  -> True_
  False_ -> y
infixr 2 ||
{-# INLINE (||) #-}

(&) :: a -> (a -> b) -> b
x & f = f x
infixl 2 &
{-# INLINE (&) #-}

begin :: W x -> x :~: x
begin _ = Refl
{-# INLINE begin #-}

-- }}}

dblNeg :: B x -> Not (Not x) :~: x
dblNeg x = begin
  (not $ not x) =<| case x of
    True_  -> Refl |> True_
    False_ -> Refl |> False_

deMorg :: B x -> B y -> Not (x && y) :~: (Not x || Not y)
deMorg x y = begin
  (not $ x && y) =<| case x of
    True_  -> Refl |> (not x || not y)
    False_ -> Refl |> True_

deMorg' :: B x -> B y -> Not (x || y) :~: (Not x && Not y)
deMorg' x y = begin
  (not $ x || y) =<| case x of
    True_  -> Refl |> False_
    False_ -> Refl |> (not x && not y)

foo :: W x -> x :~: x
foo x = begin 
  x =<| Refl |> x
    =<| Refl |> x
    =<| Refl |> x
    =<| Refl |> x
    =<| Refl |> x
    =<| Refl |> x
    =<| Refl |> x

