{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Data.Functor.Ord where

import Prelude hiding ((<=),(>=),(<),(>))
import Data.Functor.Classes.Extra
import GHC.Exts (Any)

data family (x :: k) <= (y :: k) :: *
type x >= y = y <= x

data family (x :: k) < (y :: k) :: *
type x > y = y < x

class (t ~ Any) => HOrd (t :: k) where
  leTrans :: ((a :: k) <= b) -> (b <= c) -> (a <=  c)
  ltTrans :: ((a :: k) <  b) -> (b <  c) -> (a <   c)
  leASymm :: ((a :: k) <= b) -> (b <= a) -> (a :~: b)
  ltAntiSymm :: ((a :: k) <  b) -> (b <  a) -> Void
  leRefl  :: ((a :: k) <= a)
  ltARefl :: ((a :: k) <  a) -> Void

class HOrd (Any :: k) => OrdF (t :: k -> *) where
  compareF :: t a -> t b -> Ordering
  (<:)     :: t a -> t b -> Poss2 (<)  '[ '(a,b), '(b,a) ]
  (>:)     :: t a -> t b -> Poss2 (<)  '[ '(a,b), '(b,a) ]
  (<=:)    :: t a -> t b -> Poss2 (<=) '[ '(a,b), '(b,a) ]
  (>=:)    :: t a -> t b -> Poss2 (<=) '[ '(a,b), '(b,a) ]
  (<)      :: t a -> t b -> Bool
  (>)      :: t a -> t b -> Bool
  (<=)     :: t a -> t b -> Bool
  (>=)     :: t a -> t b -> Bool
  maxF     :: t a -> t b -> Poss t '[a,b]
  minF     :: t a -> t b -> Poss t '[a,b]

