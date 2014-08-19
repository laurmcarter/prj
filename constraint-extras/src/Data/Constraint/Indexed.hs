{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Constraint.Indexed where

import Data.Constraint.Extra
import Data.Constraint.Member
import GHC.TypeLits
import GHC.Exts (Constraint)

-- Constructor Spec {{{

class HasCons
  (op :: (* -> *) -> [*] -> [*] -> *)
  where
  type Cons op :: [Symbol]
  distinguish :: op m a b -> DU Symb (Cons op)

instance HasCons RingOp where
  type Cons RingOp =
    '[ "Plus"
     , "Times"
     , "Negate"
     , "Zero"
     , "Unit"
     ]
  distinguish = \case
    Plus   -> L Symb
    Times  -> R $ L Symb
    Negate -> R $ R $ L Symb
    Zero   -> R $ R $ R $ L Symb
    Unit   -> R $ R $ R $ R $ L Symb

-- }}}

-- Constructor -> Constraint Mapping {{{

class Requires
  (op :: (* -> *) -> [*] -> [*] -> *)
  (m :: * -> *)
  (x :: Symbol)
  (ai :: [*])
  (ao :: [*])
  (bi :: [*])
  (bo :: [*])
  (ac :: Constraint)
  (bc :: Constraint)
  | op m x   ai ao            -> ac
  , op m x   bi bo            -> bc
  , op m x   ai ao ac   bi bo -> bc
  , op m x   ai ao ac   bi bc -> bo
  , op m x   ai ao ac   bc bo -> bi
  , op m x   bi bo bc   ai ao -> ac
  , op m x   bi bo bc   ai ac -> ao
  , op m x   bi bo bc   ac ao -> ai
  where

class Required
  (op :: (* -> *) -> [*] -> [*] -> *)
  (m  :: * -> *)
  (x  :: Symbol)
  (a  :: [*])
  (b  :: [*])
  where

instance (Requires op m x a b a b c c, c) => Required op m x a b

-- }}}

class IxProof (t :: *) where
  type IxProofOf t (x :: Symbol) :: Constraint
  type IxResidue t (x :: Symbol) :: Constraint
  (//*) :: IxResidue t x => Tagged x t -> (IxProofOf t x => r) -> r

-- RingOp {{{

data RingOp (m :: * -> *) :: [*] -> [*] -> * where
  Plus   :: Required RingOp m "Plus"   '[r,r] '[r] => RingOp m '[r,r] '[r]
  Times  :: Required RingOp m "Times"  '[r,r] '[r] => RingOp m '[r,r] '[r]
  Negate :: Required RingOp m "Negate"   '[r] '[r] => RingOp m   '[r] '[r]
  Unit   :: Required RingOp m "Unit"      '[] '[r] => RingOp m    '[] '[r]
  Zero   :: Required RingOp m "Zero"      '[] '[r] => RingOp m    '[] '[r]

instance Requires RingOp IO "Plus"   '[r,r] '[r] '[s,s] '[s] (Num r) (Num s)
instance Requires RingOp IO "Times"  '[r,r] '[r] '[s,s] '[s] (Num r) (Num s)
instance Requires RingOp IO "Negate"   '[r] '[r]   '[s] '[s] (Num r) (Num s)
instance Requires RingOp IO "Unit"      '[] '[r]    '[] '[s] (Num r) (Num s)
instance Requires RingOp IO "Zero"      '[] '[r]    '[] '[s] (Num r) (Num s)

{-
instance IxProof (RingOp m a b) where
  type IxProofOf (RingOp m a b) x = Required RingOp m x a b
  type IxResidue (RingOp m a b) x = ()
  Tagged rop //* r = case rop of
    Plus   -> r
    Times  -> undefined
    Negate -> undefined
    Unit   -> undefined
    Zero   -> undefined
-}

-- }}}

-- Support Types {{{

data DU (f :: k -> *) :: [k] -> * where
  L :: f a     -> DU f (a ': as)
  R :: DU f as -> DU f (a ': as)

data Symb (x :: Symbol) where
  Symb :: KnownSymbol x => Symb x

data Tagged (x :: Symbol) a = Tagged { unTag :: a }

-- }}}

