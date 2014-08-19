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

module Type.Type where

import GHC.Exts (Constraint)

data Type a = Type deriving (Eq,Show)

tAp :: Type (f :: k -> l) -> Type (a :: k) -> Type (f a)
tAp Type Type = Type

-- Product {{{

data Product a b = a :*: b
  deriving (Eq,Show)

infixr 6 :*:

type family Fst (p :: Product k l) :: k where
  Fst (a :*: b) = a
type family Snd (p :: Product k l) :: l where
  Snd (a :*: b) = b
type Diag (a :: k) = a :*: a

_Fst :: Type p -> Type (Fst p)
_Fst Type = Type

_Snd :: Type p -> Type (Snd p)
_Snd Type = Type

-- }}}

-- Coproduct {{{

data Coproduct a b
  = L a
  | R b
  deriving (Eq,Show)

type family InL (p :: Coproduct k l) :: Maybe k where
  InL (L a) = Just a
  InL (R b) = Nothing
type family InR (p :: Coproduct k l) :: Maybe l where
  InR (L a) = Nothing
  InR (R b) = Just b
type family Codiag (p :: Coproduct k k) :: k where
  Codiag (L a) = a
  Codiag (R b) = b

_InL :: Type p -> Type (InL p)
_InL Type = Type

_InR :: Type p -> Type (InR p)
_InR Type = Type

_Codiag :: Type p -> Type (Codiag p)
_Codiag Type = Type

-- }}}

-- FMap {{{

type family FMap (f :: x -> y) (m :: k1) :: k2

type instance FMap f Nothing  = Nothing
type instance FMap f (Just a) = Just (f a)

type instance FMap f '[] = '[]
type instance FMap f (a ': as) = f a ': FMap f as

-- }}}

-- If {{{

type family If (t :: Bool) (c :: k) (a :: k) :: k where
  If True  c a = c
  If False c a = a

-- }}}

-- Member {{{

class Member a as where
  inProof :: MemEvidence a as

instance Member a (a ': as) where
  inProof = Head

instance Member a as => Member a (b ': as) where
  inProof = Tail

data MemEvidence (a :: k) (as :: [k]) where
  Head :: MemEvidence a (a ': as)
  Tail :: Member a as => MemEvidence a (b ': as)

-- }}}

-- All {{{

data Trap c b where
  Trap :: c b => Trap c b

class All c as where
  withElem :: Member b as => Type as -> (Trap c b -> d) -> d

instance All c '[] where
  withElem Type (f :: Trap c b -> d) = seq (inProof :: MemEvidence b '[]) undefined

instance (c a, All c as) => All c (a ': as) where
  withElem Type (f :: Trap c b -> d) = case inProof :: MemEvidence b (a ': as) of
    Head -> f Trap
    Tail  -> withElem (Type :: Type as) f

class AllEq a as
instance AllEq a '[]
instance AllEq a as => AllEq a (a ': as)

-- }}}

