{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Data.Constraint.Base
  ( module Data.Constraint.Base
  , module Data.Constraint
  , type   Constraint
  ) where

import Data.Constraint hiding ((\\),(***))
import Data.Type.Equality

type EmptyC = (() :: Constraint)

type family Holds (c :: Constraint) :: Bool

-- Constraint and Data Composition {{{

class    c (f a) => ((c :: l -> Constraint) :.: (f :: k -> l)) (a :: k)
instance c (f a) => ((c :: l -> Constraint) :.: (f :: k -> l)) (a :: k)

type instance Holds ((c :.: f) a) = Holds (c (f a))

newtype ((f :: l -> *) .: (g :: k -> l)) (a :: k) = Comp
  { unComp :: f (g a)
  } deriving (Eq,Ord,Show)

-- }}}

-- Bool {{{

type Prop (p :: Bool) = (p ~ True)

-- | B serves as a witness to a type level Bool value.
data B (b :: Bool) where
  True_  :: B True
  False_ :: B False

instance TestEquality B where
  testEquality x y = case (x,y) of
    (True_ ,True_ ) -> Just Refl
    (False_,False_) -> Just Refl
    _               -> Nothing

-- }}}

-- Curry / Uncurry {{{

-- | U is an Uncurrying data type
data U :: (k -> l -> *) -> (k,l) -> * where
  U :: p a b -> U p '(a,b)

mapUncurry :: (p a b -> q c d) -> U p '(a,b) -> U q '(c,d)
mapUncurry f (U p) = U $ f p

ntUncurry :: (forall a b. p a b -> q a b) -> U p '(a,b) -> U q '(a,b)
ntUncurry f (U p) = U $ f p


-- | C is a Currying data type
data C :: ((k,l) -> *) -> k -> l -> * where
  C :: p '(a,b) -> C p a b

mapCurry :: (p '(a,b) -> q '(c,d)) -> C p a b -> C q c d
mapCurry f (C p) = C $ f p

ntCurry :: (forall a b. p '(a,b) -> q '(a,b)) -> C p a b -> C q a b
ntCurry f (C p) = C $ f p

-- }}}

unreachable :: a
unreachable = error "unreachable"

