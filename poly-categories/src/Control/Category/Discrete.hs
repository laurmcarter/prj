{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Category.Discrete where

import qualified Prelude
import Prelude hiding (id,(.),fst,snd,curry,uncurry)

import Data.Semigroupoid
import Control.Categorical

data Discrete (a :: k) (b :: k) where
  Refl :: Discrete a a

instance Semigroupoid Discrete where
  Refl . Refl = Refl

instance Category Discrete where
  id = Refl

(@:) :: Discrete f g -> Discrete a b -> Discrete (f a) (g b)
Refl @: Refl = Refl
infixl 4 @:

liftDiscrete :: Discrete a b -> Discrete (f a) (f b)
liftDiscrete Refl = Refl

cast :: Category k => Discrete a b -> k a b
cast Refl = id

inverse :: Discrete a b -> Discrete b a
inverse Refl = Refl

