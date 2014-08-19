{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Semigroupoid where

import Prelude hiding (id,(.))
import qualified Prelude

class Semigroupoid (sem :: k -> k -> *) where
  (.) :: sem b c -> sem a b -> sem a c
  infixr 9 .

instance Semigroupoid (->) where
  bc . ab = bc Prelude.. ab

instance Semigroupoid (,) where
  (_,c) . (a,_) = (a,c)

