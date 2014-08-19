{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.I where

import Prelude hiding (div,mod,fst,snd)
import Data.Proxy
import Data.Proxy.TH

data I
  = N I
  | Z
  | S I
  deriving (Eq)

int :: I -> Int
int i = case i of
  Z -> 0
  S i' -> succ $ int i'
  N i' -> pred $ int i'

natV :: I -> Int
natV n = case n of
  Z -> 0
  S n' -> succ $ natV n'
  _ -> error "Not a natural number"

instance Show I where
  showsPrec d n = showParen (d > 5)
    $ showString "I "
    . shows (natV n)

-- Natural {{{

class             Nat (n :: I) where
  nat :: Proxy n -> Int
instance          Nat Z        where
  nat _ = 0
instance Nat n => Nat (S n)    where
  nat _ = 1 + nat (Proxy :: Proxy n)

-- }}}

-- Succ Pred {{{

type family Succ (n :: I) :: I where
  Succ (N n) = n
  Succ    Z  = S Z
  Succ (S n) = S (S n)

type family Pred (n :: I) :: I where
  Pred (N n) = N (N n)
  Pred    Z  = N Z
  Pred (S n) = n

-- }}}

-- Add Sub {{{

type family Add (m :: I) (n :: I) :: I where
  Add    Z     Z  = Z
  Add    Z  (S n) = S n
  Add    Z  (N n) = N n
  Add (S m)    Z  = S m
  Add (S m) (S n) = S (S (Add m n))
  Add (S m) (N n) = Add m n
  Add (N m)    Z  = N m
  Add (N m) (S n) = Add m n
  Add (N m) (N n) = N (N (Add m n))

type family Sub (m :: I) (n :: I) :: I where
  Sub     Z     Z = Z
  Sub     Z (S n) = N (Sub Z n)
  Sub     Z (N n) = S (Sub Z n)
  Sub (S m)     Z = S m
  Sub (S m) (S n) = Sub m n
  Sub (S m) (N n) = S (S (Sub m n))
  Sub (N m)     Z = N m
  Sub (N m) (S n) = N (N (Sub m n))
  Sub (N m) (N n) = Sub m n

-- }}}

-- Mul Div {{{

type family Mul (m :: I) (n :: I) :: I where
  Mul    Z     n  = Z
  Mul    m     Z  = Z
  Mul (S m) (S n) = Add (Mul m (S n)) (S n)
  Mul (S m) (N n) = Add (Mul m (N n)) (N n)
  Mul (N m) (S n) = Sub (Mul m (S n)) (S n)
  Mul (N m) (N n) = Sub (Mul m (N n)) (N n)

type family Div (m :: I) (n :: I) :: I where
  Div    Z  (S n) = Z
  Div    Z  (N n) = Z
  Div (S m) (S n) = Succ (Div (Sub (S m) (S n)) (S n))
  Div (S m) (N n) = Pred (Div (Add (S m) (N n)) (N n))
  Div (N m) (S n) = Pred (Div (Add (N m) (S n)) (S n)) 
  Div (N m) (N n) = Succ (Div (Sub (N m) (N n)) (N n)) 

-- }}}

-- Combination {{{

type family Choose (n :: I) (k :: I) :: I where
  Choose    Z     Z  = S Z
  Choose (S n)    Z  = S Z
  Choose    Z  (S k) = Z
  Choose (S n) (S k) = Add (Choose n (S k)) (Choose n k)

-- }}}

-- Low Dim {{{

type NegNine  = N NegEight
type NegEight = N NegSeven
type NegSeven = N NegSix
type NegSix   = N NegFive
type NegFive  = N NegFour
type NegFour  = N NegThree 
type NegThree = N NegTwo
type NegTwo   = N NegOne
type NegOne   = N Zero
type Zero     = Z
type One      = S Zero
type Two      = S One
type Three    = S Two
type Four     = S Three 
type Five     = S Four
type Six      = S Five
type Seven    = S Six
type Eight    = S Seven
type Nine     = S Eight

iNegNine   = Proxy :: Proxy NegNine
iNegEight  = Proxy :: Proxy NegEight
iNegSeven  = Proxy :: Proxy NegSeven
iNegSix    = Proxy :: Proxy NegSix
iNegFour   = Proxy :: Proxy NegFour
iNegFive   = Proxy :: Proxy NegFive
iNegThree  = Proxy :: Proxy NegThree
iNegTwo    = Proxy :: Proxy NegTwo
iNegOne    = Proxy :: Proxy NegOne
iZero      = Proxy :: Proxy Zero
iOne       = Proxy :: Proxy One
iTwo       = Proxy :: Proxy Two
iThree     = Proxy :: Proxy Three
iFour      = Proxy :: Proxy Four
iFive      = Proxy :: Proxy Five
iSix       = Proxy :: Proxy Six
iSeven     = Proxy :: Proxy Seven
iEight     = Proxy :: Proxy Eight
iNine      = Proxy :: Proxy Nine

-- }}}

{-
[proxify|
  suc    ::      n ->       Succ n
  prd    ::      n ->       Pred n
  add    :: m -> n ->    m `Add` n
  sub    :: m -> n ->    m `Sub` n
  mul    :: m -> n ->    m `Mul` n
  div    :: m -> n ->    m `Div` n
  choose :: n -> k -> n `Choose` k
|]
-}

