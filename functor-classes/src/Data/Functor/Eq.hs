{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module Data.Functor.Eq where

import Prelude hiding ((==),(/=))
import Data.Type.Equality
import Data.Void

class EqF t where
  (==:) :: t a -> t b -> Maybe  (a :~: b)
  (/=:) :: t a -> t b -> Maybe ((a :~: b) -> Void)
  (==)  :: t a -> t b -> Bool
  x == y = case x ==: y of
    Just _ -> True
    _      -> False
  (/=)  :: t a -> t b -> Bool
  x /= y = case x /=: y of
    Just _ -> True
    _      -> False
infixr 4 ==
infixr 4 /=
infixr 4 ==:
infixr 4 /=:

