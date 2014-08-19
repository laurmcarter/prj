{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Constraint.Evidence where

import Data.Constraint.Dict

import Data.Proxy
import GHC.Prim (Constraint)

class Evidence0 (c :: Constraint) where
  data Ev0 c
  impl0 :: Ev0 c -> Dict c
  expl0 :: Dict c -> Ev0 c

class Evidence1 (c :: k -> Constraint) where
  data Ev1 c (a :: k)
  impl1 :: Ev1 c a -> Dict1 c a
  expl1 :: Dict1 c a -> Ev1 c a

class Evidence2 (c :: k -> l -> Constraint) where
  data Ev2 c (a :: k) (b :: l)
  impl2 :: Ev2 c a b -> Dict2 c a b
  expl2 :: Dict2 c a b -> Ev2 c a b

