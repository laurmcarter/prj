{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Data.Profunctor.Higher where

import Data.Functor.Higher

class HProfunctor (p :: k -> l -> *) where
  type LArr p (a :: k) (b :: k) :: *
  type RArr p (a :: l) (b :: l) :: *
  lmap  :: LArr p a b -> p b c -> p a c
  rmap  :: RArr p b c -> p a b -> p a c
  dimap :: LArr p b c -> RArr p c d -> p b c -> p a d

