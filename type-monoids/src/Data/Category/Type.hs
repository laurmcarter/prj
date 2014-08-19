{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Category.Type where

import Data.Monoid.Type (Sing,c3)
import Data.Proxy
import GHC.Exts (Any)
import Data.Type.Equality
import Control.Applicative

class (f ~ Any) => TCategory (f :: (obj,arr)) where
  type Id   f (x :: obj)            :: arr
  type Comp f (g :: arr) (h :: arr) :: arr
  type Arr  f                       :: obj -> obj -> arr
  id_ :: Sing x -> Sing (Id f x)
  (∘) :: (Arr f x y ~ xy, Arr f y z ~ yz)
    => Sing yz -> Sing xy
    -> Sing (Comp f xy yz)

{-
-- {{{
data IdLeft  (f :: (obj,arr)) (x :: obj) (y :: obj) = IdLeft
  { unIdLeft :: !(C f (A f x x) (A f x y) :~: A f x y)
  }

data IdRight (f :: (obj,arr)) (x :: obj) (y :: obj) = IdRight
  { unIdRight :: !(C f (A f x y) (A f y y) :~: A f x y)
  }

data Assoc (f :: (obj,arr))
  (w :: obj) (x :: obj) (y :: obj) (z :: obj) = Assoc
  { unAssoc ::
    !(  C f (A f w x) (C f (A f x y) (A f y z))
    :~: C f (C f (A f w x) (A f x y)) (A f y z)
    )
  }

data CategoryEvidence (f :: (obj,arr)) = CategoryEvidence
  { idLeftLaw  :: forall x y.
       Sing x -> Sing y
    -> IdLeft f x y
  , idRightLaw :: forall x y.
       Sing x -> Sing y
    -> IdRight f x y
  , assocLaw   :: forall w x y z.
       Sing w -> Sing x -> Sing y -> Sing z
    -> Assoc f w x y z
  }

class (tag ~ Any, TCategory tag) => TCategoryVerified (tag :: (obj,arr)) where
  categoryLaws :: CategoryEvidence tag
-- }}}
-}

data instance Sing (x :: (k,k)) where
  (:*:) :: Sing (x :: k) -> Sing (y :: k) -> Sing '(x,y)
infixr 4 :*:

type Fam (f :: k -> *) = Sing f
data instance Sing (x :: k -> *) where
  Induct :: (forall x. (forall x. Sing (x :: k) -> f x) -> Sing (x :: k) -> f x) -> Sing f

instance (f ~ Any) => TCategory (f :: (obj,(obj,obj))) where
  type Id   f x             = '(x,x)
  type Comp f '(x,y) '(y,z) = '(x,z)
  type Arr  f               = '(,)
  id_ x = x :*: x
  (_ :*: z) ∘ (x :*: _) = x :*: z

data N
  = Z
  | S N

type family (x :: N) + (y :: N) :: N where
  Z   + y = y
  S x + y = S (x + y)

data instance Sing (x :: N) where
  Z_ :: Sing Z
  S_ :: Sing n -> Sing (S n)

data instance Sing (x :: Bool) where
  True_  :: Sing True
  False_ :: Sing False

data instance Sing (x :: Maybe k) where
  Nothing_ :: Sing Nothing
  Just_    :: Sing (x :: k) -> Sing (Just x)

three :: Sing (S (S (S Z)))
three = S_ $ S_ $ S_ Z_

checkProof :: Sing (n :: N) -> n + Z :~: n
checkProof x = case add0 @: x of
  Add0 eq -> eq

(@:) :: forall f x. Fam (f :: k -> *) -> Sing (x :: k) -> f x
(@:) (Induct f) = go
  where
  go :: forall x. Sing (x :: k) -> f x
  go = f go

add0 :: Fam Add0
add0 = Induct $ \f x -> case x of
  Z_    -> Add0 Refl
  S_ x' -> case f x' of
    Add0 (apply (Refl :: S :~: S) -> eq) -> Add0 eq

newtype Add0 (x :: N) = Add0 (x + Z :~: x)
newtype Adding (c :: N -> *) (x :: N) (y :: N) = Adding (c (x + y))
newtype Cong x y f = Cong (f x :~: f y)

{-

instance (tag ~ Any) => TCategory (tag :: (obj,(obj,obj))) where
  type IdOf   tag x             = '(x,x)
  type C tag '(x,y) '(y,z) = '(x,z)
  type A  tag x y           = '(x,y)
  id_  x                 = x :*: x
  arr_ x y               = x :*: y
  (y :*: z) ∘ (x :*: y') = _

{-
instance (tag ~ Any) => TCategoryVerified (tag :: (obj,(obj,obj))) where
  categoryLaws = CategoryEvidence
    { arrowFormLaw = ArrForm . goArrForm
    , idLeftLaw    = c2 IdLeft  goIdL
    , idRightLaw   = c2 IdRight goIdR
    , assocLaw     = c4 Assoc   goAssoc
    }
    where
    goArrForm :: forall (x :: obj). Sing x -> A tag x x :~: IdOf tag x
    goArrForm x =
      case (id_ x :: Sing (IdOf tag x), arr_ x x :: Sing (A tag x x)) of
      (i,a) -> undefined
    ----
    goIdL :: forall x y. Sing x -> Sing y
      -> (C tag (IdOf tag x) (A tag x y)) :~: (A tag x y)
    goIdL x y = undefined
    ----
    goIdR :: forall x y. Sing x -> Sing y
      -> (Id y ∘ x --> y) :~: (x --> y)
    goIdR = undefined
    ----
    goAssoc :: forall w x y z. Sing w -> Sing x -> Sing y -> Sing z
      -> (y --> z ∘ (x --> y  ∘ w --> x)) :~: ((y --> z ∘  x --> y) ∘ w --> x)
    goAssoc = undefined
-}

{-
instance (tag ~ Any) => TMonoidVerified (tag :: [k]) where
  monoidLaws = MonoidEvidence
    { idLeftLaw  =    IdLeft  . goIdL
    , idRightLaw =    IdRight . goIdR
    , assocLaw   = c3 Assoc     goAssoc
    }
    where
    goIdL :: forall as. List as -> (as <> M0) :~: as
    goIdL as = case as of
      Nil      -> Refl
      a :* as' -> apply Refl $ goIdL as'
    ----
    goIdR :: forall as. List as -> (M0 <> as) :~: as
    goIdR as = case as of
      Nil      -> Refl
      a :* as' -> apply Refl $ goIdR as'
    ----
    goAssoc :: forall (as :: [k]) bs cs. List as -> List bs -> List cs
      -> as <> (bs <> cs) :~: (as <> bs) <> cs
    goAssoc as bs cs = case as of
      Nil      -> Refl
      a :* as' -> apply Refl $ goAssoc as' bs cs
-}
c2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
c2 f g = curry $ f . uncurry g

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 fn (a,b,c,d) = fn a b c d

curry4 :: ((a,b,c,d) -> e) -> a -> b -> c -> d -> e
curry4 fn a b c d = fn (a,b,c,d)

c4 :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
c4 f g = curry4 $ f . uncurry4 g
-}

