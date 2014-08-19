{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}

module Data.Type.Equality.Leibniz where

import Control.Category
import Prelude hiding (id,(.))
import qualified Prelude as P
import Control.Lens
import Data.Void

data (a :: k) :=: (b :: k) = Equal
  { subst :: forall (f :: k -> *). f a -> f b
  }
infixr 4 :=:

refl :: a :=: a
refl = Equal id

type Not a = a -> Void

exfalso :: a -> Not a -> b
exfalso a neg = absurd $ neg a

unreachable :: a
unreachable = error "Should be unreachable"

newtype Id (a :: *) = Id
  { unId :: a
  } deriving (Eq,Show)

_Id :: Iso a b (Id a) (Id b)
_Id = iso Id unId

newtype L (p :: l -> *) (a :: k) (f :: k -> l) = L
  { unL :: p (f a)
  } deriving (Eq,Show)

_L :: Iso (p (f a)) (q (g b)) (L p a f) (L q b g)
_L = iso L unL

newtype R (f :: k -> l) (a :: k) (p :: l -> *) = R
  { unR :: p (f a)
  } deriving (Eq,Show)

_R :: Iso (p (f a)) (q (g b)) (R f a p) (R g b q)
_R = iso R unR

newtype Symm (p :: k -> l -> *) (b :: l) (a :: k) = Symm
  { unSymm :: p a b
  } deriving (Eq,Show)

_Symm :: Iso (p a b) (q c d) (Symm p b a) (Symm q d c)
_Symm = iso Symm unSymm

because :: (Id (a :=: a) -> Id b) -> b
because f = refl & over _Id f

-- | Apply an equality under a lens
(-:) :: ASetter s t (f a) (f b) -> a :=: b -> s -> t
l -: p = l %~ subst p
infixr 4 -:

