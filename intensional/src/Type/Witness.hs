{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}

module Type.Witness where

import Type.Type

class Witness (t :: k) where
  type Wit t :: k -> *
  witness :: Wit t t

type WitBy t w = (Witness t, Wit t ~ w)

type family Wits p :: [k -> *] where
  Wits (w x -> p) = w ': Wits p
  Wits p = '[]

class AllWit (w :: k -> *) (as :: [k])
instance AllWit w '[]
instance (AllWit w as, WitBy a w) => AllWit w (a ': as)

