{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Data.Type.Known
  ( module Data.Type.Known
  , Constraint , Any
  ) where

import Control.Lens
import GHC.Exts (Constraint , Any)
import Data.Type.Equality

class Known (t :: k -> *) (x :: k) where
  known :: t x

data Dict (c :: Constraint) where
  Dict :: c => Dict c

unreachable :: a
unreachable = error "unreachable"

