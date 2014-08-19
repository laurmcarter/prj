{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Language.STLC.HOPoly where

import Prelude hiding (sum)
import GHC.TypeLits

class Exp (r :: * -> *) where
  lam :: (r a  -> r b) ->  r (a -> b)
  app ::  r (a -> b)   -> (r a  -> r b)

class Arith (r :: * -> *) where
  int  ::   Int -> r Int
  (+:) :: r Int -> r Int -> r Int
  (-:) :: r Int -> r Int -> r Int
  (*:) :: r Int -> r Int -> r Int

type family RunType (r :: * -> *) (t :: *) :: *

data T (r :: * -> *) (t :: *) (k :: *) = T
  { typ :: r (RunType r t)
  }

data Star
data (f :: *) :@  (a :: *)
data (a :: *) :-> (b :: *)
data Forall (a :: *) (k :: *) (t :: *)
data Arr
data Prod
data Sum

type family RunKind (r :: * -> *) (k :: *) :: *

data K (r :: * -> *) (k :: *) (s :: *) = K
  { knd :: r (RunKind r k)
  }

data Box = Box

class Type (r :: * -> *) where
  (.::) ::  T r a k              -> K r k Box  -> T r a k
  (@:)  ::  T r f (k :-> l)      -> T r a k    -> T r (f :@ a) l
  arr   ::  T r Arr  (Star :-> Star :-> Star)
  prod  ::  T r Prod (Star :-> Star :-> Star)
  sum   ::  T r Sum  (Star :-> Star :-> Star)
  tAbs  :: (T r a k -> T r b l)  -> T r (Forall a k b) l
  tInst ::  T r (Forall a k b) l -> T r a k    -> T r b l
  infixl 8 @:
  infix  2 .::

(-->) :: Type r => T r a Star -> T r b Star -> T r (a -> b) Star
a --> b = arr @: a @: b

class Kind (r :: * -> *) where
  star  :: K r Star Box
  (*->) :: K r k Box -> K r l Box -> K r (k -> l) Box
  infixr 3 *->

-- Convenience Syntax {{{

type (t :: *) ::: (k :: *) = '(t,k)

type family Quant (xs :: [(*,*)]) (t :: *) :: * where
  Quant '[] t = t
  Quant ('(x,k) ': xs) t = Forall x k (Quant xs t)

-- }}}

-- Examples {{{

{-
typ0 :: Type r => T r (Quant '[a:::Star,b:::Star] (a -> b)) Star
typ0 = tAbs $ \a -> tAbs $ \b -> a --> b
-}

{-
knd0 :: Kind r => K r Star Box
knd0 = star

knd1 :: Kind r => K r (Star -> Star -> Star) Box
knd1 = star *-> star *-> star
-}

-- }}}

