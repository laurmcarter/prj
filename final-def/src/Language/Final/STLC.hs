{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module Language.Final.STLC where

import GHC.Exts (Constraint)
import GHC.TypeLits
import Data.Void
import Data.Type.Bool
import Data.Type.Equality

{-
class Def (r :: [(Symbol,*,Bool)] -> [(Symbol,*)] -> * -> *) where
  (.=) :: Id x
-}

data Id (x :: Symbol) = Id

