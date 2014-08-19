{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Categorical.Object where

import qualified Prelude
import Prelude hiding (id,(.))

import Data.Semigroupoid
import Control.Categorical
import Control.Category.Dual

class Category k => HasTerminalObject (k :: x -> x -> *) where
  type Terminal k :: x
  terminate :: a `k` Terminal k

class Category k => HasInitialObject (k :: x -> x -> *) where
  type Initial k :: x
  initiate :: Initial k `k` a

