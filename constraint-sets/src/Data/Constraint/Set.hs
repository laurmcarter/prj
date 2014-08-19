{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Data.Constraint.Set where

import Data.Constraint hiding (weaken)
import Data.Proxy

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family All (c :: [Constraint]) :: Constraint where
  All '[]       = ()
  All (c ': cs) = (c,All cs)

data Dicts (as :: [Constraint]) where
  Done :: Dicts '[]
  Have :: Dict a -> Dicts as -> Dicts (a ': as)

data (as :: [Constraint]) :-- (bs :: [Constraint]) where
  Ready :: Dicts bs -> ('[] :-- bs)
  Need  :: (Dict  a -> (as :-- bs)) -> (a ': as) :-- bs
infixr 5 :--

-- Forget / Weaken / Subsume {{{

class Forget (as :: [Constraint]) (bs :: [Constraint]) where
  forget  :: Dicts as -> Dicts bs
  subsume :: (bs :-- cs) -> (as :-- cs)

instance Forget '[] '[] where
  forget Done       = Done
  subsume (Ready d) = Ready d

instance (IsCons bs p, Forget' (a ': as) bs p) => Forget (a ': as) bs where
  forget  = forget' (Proxy :: Proxy p)
  subsume = subsume' (Proxy :: Proxy p)

class Forget' (as :: [Constraint]) (bs :: [Constraint]) (p :: Bool) where
  forget'  :: prx p -> Dicts as -> Dicts bs
  subsume' :: prx p -> (bs :-- cs) -> (as :-- cs)

instance Forget as bs => Forget' (a ': as) (a ': bs) True where
  forget'  _ (Have d as) = Have d $ forget as
  subsume' _ (Need f)    = Need $ subsume . f

instance Forget as bs => Forget' (a ': as) bs False where
  forget'  _ (Have d as) = forget as
  subsume' _             = Need . const . subsume

weaken :: Forget bs bs' => (as :-- bs) -> (as :-- bs')
weaken = \case
  Ready d -> Ready $ forget   d
  Need  f -> Need  $ weaken . f

class CondIns
  (p  :: Bool -> Constraint)
  (br :: Bool -> Constraint)
  (b :: Constraint)
  (h :: Constraint) | p h -> br b where
  trueIns  :: (p True ,br True ,b) :- h
  falseIns :: (p False,br False,b) :- h

instance CondIns (IsCons bs) (Forget' (a ': as) bs) (Forget as bs) (Forget (a ': as) bs) where
  trueIns  = Sub Dict
  falseIns = Sub Dict

class IsCons (as :: [k]) (p :: Bool) | as -> p
instance (p ~ True ) => IsCons (a ': as) p
instance (p ~ False) => IsCons '[]       p

instance Class () (Forget as bs) where cls = Sub Dict
instance () :=> Forget '[] '[] where ins = Sub Dict

instance Class () (IsCons as p)             where cls = Sub Dict
instance (p ~ True ) :=> IsCons (a ': as) p where ins = Sub Dict
instance (p ~ False) :=> IsCons '[]       p where ins = Sub Dict

-- }}}

-- Reify / Reflect {{{

reflect1 :: a => (a ': as) :-- bs -> as :-- bs
reflect1 (Need f) = f Dict

reflect :: All as => (as :-- bs) -> Dicts bs
reflect = \case
  Ready ds -> ds
  Need  f  -> reflect $ f Dict

reify1 :: (a => as :-- bs) -> (a ': as) :-- bs
reify1 s = Need $ \Dict -> s

class Reify (as :: [Constraint]) where
  reify :: (All as => Dicts cs) -> as :-- cs

instance Reify '[] where
  reify cs = Ready cs

instance Reify as => Reify (a ': as) where
  reify cs = Need $ \Dict -> reify cs

-- }}}

{-
(\\:) :: All as => (All bs => r) -> (as :-- bs) -> r
r \\: Subs ds = discharge ds r
infixl 1 \\:

(.//) :: All as => (as :-- bs) -> (All bs => r) -> r
Subs ds .// r = discharge ds r
infixr 1 .//
-}

discharge1 :: Dicts (a ': as) -> (a => r) -> (r,Dicts as)
discharge1 (Have Dict as) r = (r,as)

discharge :: Dicts as -> (All as => r) -> r
discharge as r = case as of
  Done          -> r
  Have Dict as' -> discharge as' r

joinDicts :: Dicts as -> Dicts bs -> Dicts (as ++ bs)
joinDicts as bs = case as of
  Done       -> bs
  Have d as' -> Have d $ joinDicts as' bs

merge :: Reify as => (as :-- bs) -> (as :-- cs) -> (as :-- (bs ++ cs))
merge ab ac = reify
  $ let bs = reflect ab
        cs = reflect ac
    in joinDicts bs cs

{-
graft :: (asbs ~ (as ++ bs), Reify asbs) => (as :-- cs) -> (bs :-- cs) -> ((as ++ bs) :-- cs)
graft ac bc = reify $ reflect ac
-}

{-
(***:) :: (as :-- bs) -> (cs :-- ds) -> (as ++ cs) :-- (bs ++ ds)
f@(Subs ab) ***: g@(Subs cd) = Subs $ f .// g .// joinDicts 
-}

