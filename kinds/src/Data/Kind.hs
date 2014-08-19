{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Data.Kind where

import Data.Type.Bool
import Data.Type.Equality hiding (apply)
import Data.Proxy
import GHC.Exts (Constraint)

-- Logical Types {{{

type (a :: k) ∈ (s :: k -> l) = s a
infixr 6 ∈

data ((r :: k -> *) ∪ (s :: k -> *)) a
  = L (a ∈ r)
  | R (a ∈ s)

disj :: (a ∈ r -> b) -> (a ∈ s -> b) -> a ∈ (r ∪ s) -> b
disj f g p = case p of
  L r -> f r
  R s -> g s

data ((r :: k -> *) ∩ (s :: k -> *)) a = Conj
  { conjL :: a ∈ r
  , conjR :: a ∈ s
  }

conj :: (a ∈ r -> a ∈ s -> b) -> a ∈ (r ∩ s) -> b
conj f (Conj r s) = f r s

data ((r :: k -> *) ⊃ (s :: k -> *)) (a :: k) = Impl
  { impl :: a ∈ r -> a ∈ s
  }

data Forall (s :: k -> *) (p :: k -> *) = Forall
  { univ :: forall (a :: k). a ∈ s -> p a
  }

data SuchThat (c :: Constraint) (p :: *) = ST
  { suchThat :: c => p
  }

data Exists (s :: k -> *) (p :: k -> *) = forall (a :: k). Exists
  { exist :: (a ∈ s, p a)
  }

data ((r :: l -> *) ∘ (s :: k -> l)) (a :: k) = Comp
  { comp :: (a ∈ s) ∈ r
  }

data Truth   = Truth
data Falsity = Falsity !Falsity

-- }}}

type family (as :: [k]) ∈* (s :: k -> *) :: [*] where
  '[]       ∈* s = '[]
  (a ': as) ∈* s = s a ': (as ∈* s)
infixr 6 ∈*

type family (ts :: [*]) --> (r :: *) :: * where
  '[]       --> r = r
  (t ': ts) --> r = t -> (ts --> r)
infixr 4 -->

type Star = Id

star :: Star a -> a
star = runId

class Generate (knd :: k -> *) (a :: k) where
  auto :: knd a

-- Member {{{

class IsMember (a :: k) (as :: [k]) where
  member :: Member a as

instance ((a == b) ~ eq, IsMember' eq a (b ': as))
  => IsMember a (b ': as) where
  member = member' (P :: P eq)

class IsMember' (eq :: Bool) (a :: k) (as :: [k]) where
  member' :: px eq -> Member a as

instance (a ~ b) => IsMember' True a (b ': as) where
  member' _ = Head

instance IsMember a as => IsMember' False a (b ': as) where
  member' _ = Tail

data Member (a :: k) (as :: [k]) where
  Head :: Member a (a ': as)
  Tail :: IsMember a as
       => Member a (b ': as)

deriving instance Show (Member a as)

emptyList :: Member a '[] -> b
emptyList _ = error "Invalid Member Proof"

-- }}}

-- All {{{

class All (c :: k -> Constraint) (as :: [k]) where
  allInst :: IsMember b as
          => px as -> (C (c b) -> r) -> r

instance All c '[] where
  allInst _ (f :: C (c b) -> r) = f $ emptyList (member :: Member b '[])

instance (c a, All c as) => All c (a ': as) where
  allInst _ (f :: C (c b) -> r) =
    case (member :: Member b (a ': as)) of
      Head -> f C
      Tail -> allInst (P :: P as) f

-- }}}

-- Each {{{

class Each (cs :: [k -> Constraint]) (a :: k) where
  eachInst :: IsMember c cs
           => prx cs -> (C (c a) -> r) -> r

instance Each '[] a where
  eachInst _ (f :: C (c a) -> r) = f $ emptyList (member :: Member c '[])

instance (c a, Each cs a) => Each (c ': cs) a where
  eachInst _ (f :: C (d a) -> r) =
    case (member :: Member d (c ': cs)) of
      Head -> f C
      Tail -> eachInst (P :: P cs) f

-- }}}

newtype Id a = Id { runId :: a }

data C (c :: Constraint) where
  C :: c => C c

type    P = Proxy
pattern P = Proxy

