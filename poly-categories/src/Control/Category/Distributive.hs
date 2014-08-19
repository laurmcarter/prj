{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Category.Distributive where

import qualified Prelude
import Prelude hiding (id,(.),fst,snd,curry,uncurry)

import Data.Semigroupoid
import Control.Categorical
import Control.Categorical.Bifunctor
import Control.Category.Cartesian

class (Cartesian k, CoCartesian k)
  => Distributive (k :: x -> x -> *) where
  distribute :: Product k a (Sum k b c)
     `k` Sum k (Product k a b)
               (Product k a c)

instance Distributive (->) where
  distribute = \case
    (a,Left  b) -> Left  (a,b)
    (a,Right c) -> Right (a,c)

factor :: (Cartesian k, CoCartesian k)
   => Sum k (Product k a b) (Product k a c)
  `k` Product k a (Sum k b c)
factor = second inl ||| second inr

