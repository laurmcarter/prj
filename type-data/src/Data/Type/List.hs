{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Type.List where

import Data.Type.Known
import Data.Type.Nat as Nat
import qualified Data.Type.Vec as Vec
import Data.Type.Equality

import Prelude as P hiding ((++),reverse,length,repeat,(!!))
import Control.Applicative
import Control.Monad
import Data.Proxy
import Data.Functor.Identity
import Data.Functor.Constant

data List (as :: [*]) where
  Nil  :: List '[]
  Cons :: a -> List as -> List (a ': as)

-- Eq Show {{{

instance All Eq as => Eq (List as) where
  Nil == Nil = True
  Cons (a :: a) (as :: List as') == Cons b bs
    = withAll (Proxy :: Proxy as)
    $ \(Dict :: Dict (Eq a))
       (Dict :: Dict (All Eq as'))
   -> a == b && as == bs
  _ == _ = unreachable

instance All Show as => Show (List as) where
  showsPrec d as = case as of
    Nil        -> showString "Nil"
    Cons (a :: a) (as' :: List as') -> withAll (Proxy :: Proxy as)
      $ \(Dict :: Dict (Show a))
         (Dict :: Dict (All Show as'))
     -> showParen (d > 10)
      $ showString "Cons "
      . showsPrec 11 a
      . showChar ' '
      . showsPrec 11 as'

-- }}}

-- Known {{{

instance Known List ('[] :: [*]) where
  known = Nil

instance Known List as => Known List ((() ': as) :: [*]) where
  known = Cons () known

-- }}}

-- Append {{{

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys       = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

(++) :: List as -> List bs -> List (as ++ bs)
as ++ bs = case as of
  Nil        -> bs
  Cons a as' -> Cons a $ as' ++ bs

-- }}}

-- Snoc {{{

type family Snoc (xs :: [k]) (x :: k) :: [k] where
  Snoc '[]       x = '[x]
  Snoc (y ': xs) x = y ': Snoc xs x

snoc :: List as -> a -> List (Snoc as a)
snoc as a = case as of
  Nil        -> Cons a Nil
  Cons b as' -> Cons b $ snoc as' a

-- }}}

-- Reverse {{{

type family Reverse (xs :: [k]) :: [k] where
  Reverse '[]       = '[]
  Reverse (x ': xs) = Snoc (Reverse xs) x

reverse :: List as -> List (Reverse as)
reverse as = case as of
  Nil        -> Nil
  Cons a as' -> snoc (reverse as') a

-- }}}

-- Length {{{

type family Length (as :: [k]) :: N where
  Length '[]       = Z
  Length (a ': as) = S (Length as)

length :: List as -> Nat (Length as)
length as = case as of
  Nil        -> Z_
  Cons _ as' -> S_ $ length as'

-- }}}

-- Member {{{

data MemEvidence (a :: k) (as :: [k]) where
  Head :: MemEvidence a (a ': as)
  Tail :: Member a as => Proxy as -> MemEvidence a (b ': as)

class Member (a :: k) (as :: [k]) where
  memEvidence :: MemEvidence a as

noMembers :: MemEvidence a '[] -> b
noMembers _ = error "No members in empty list"

instance ((a == b) ~ r, Member' r a (b ': as)) => Member a (b ': as) where
  memEvidence = memEvidence' (Proxy :: Proxy r)

class Member' (r :: Bool) (a :: k) (as :: [k]) where
  memEvidence' :: prx r -> MemEvidence a as

instance (a ~ b) => Member' True (a :: k) (b ': as) where
  memEvidence' _ = Head

instance Member a as => Member' False (a :: k) (b ': as) where
  memEvidence' _ = Tail Proxy

-- }}}

-- All {{{

data AllEvidence (c :: k -> Constraint) (as :: [k]) where
  AllNil  :: AllEvidence c '[]
  AllCons :: Dict (c a) -> Dict (All c as) -> AllEvidence c (a ': as)

class All (c :: k -> Constraint) (as :: [k]) where
  allEvidence :: AllEvidence c as
  withElem :: Member b as => prx as -> (Dict (c b) -> r) -> r
  withElem _ (f :: Dict (c b) -> r) = case (me,ae) of
    (Head  ,AllCons d _) -> f d
    (Tail p,AllCons _ Dict) -> withElem p f
    _ -> unreachable
    where
    me :: MemEvidence b as
    me = memEvidence
    ae :: AllEvidence c as
    ae = allEvidence

withAll :: forall as a as' c prx r.  (as ~ (a ': as'), All c as)
  => prx as -> (Dict (c a) -> Dict (All c as') -> r) -> r
withAll _ f = case (allEvidence :: AllEvidence c as) of
  AllCons da das -> f da das
  _ -> unreachable

withRest :: forall c as a as' prx r. (as ~ (a ': as'), All c as)
  => prx as -> (Dict (All c as') -> r) -> r
withRest _ f = case (allEvidence :: AllEvidence c (a ': as')) of
  AllCons _ das -> f das

instance All c '[] where
  allEvidence = AllNil

instance (c a, All c as) => All c (a ': as) where
  allEvidence = AllCons Dict Dict

-- }}}

-- Repeat {{{

type family Repeat (x :: N) (a :: k) :: [k] where
  Repeat Z     a = '[]
  Repeat (S x) a = a ': Repeat x a

repeat :: Nat x -> a -> List (Repeat x a)
repeat x a = case x of
  Z_    -> Nil
  S_ x' -> Cons a $ repeat x' a

-- }}}

-- NonEmpty {{{

newtype NonEmpty as a = NonEmpty
  { nonEmpty :: List (a ': as)
  }

deriving instance (Eq   a, All Eq   as) => Eq   (NonEmpty as a)
deriving instance (Show a, All Show as) => Show (NonEmpty as a)

pattern NE a as = NonEmpty (Cons a as)

instance Functor (NonEmpty as) where
  fmap f (NE a as) = NE (f a) as
  fmap _ _         = unreachable

newtype Needs r a = Needs
  { needs :: a -> r
  }

instance Known (Needs (NonEmpty '[] a)) a where
  known = Needs $ \a -> NE a Nil

instance Known (Needs (NonEmpty as a)) n
  => Known (Needs (NonEmpty (a ': as) b)) (b,n) where
  known = Needs $ \(b,n) ->
    let l :: Needs (NonEmpty as a) n
        l = known
        NonEmpty x = needs l n
    in NonEmpty $ Cons b x

instance Known List as => Applicative (NonEmpty as) where
  pure  = return
  (<*>) = ap

instance Known List as => Monad (NonEmpty as) where
  return a = NE a known
  NE a as >>= f = f a
  _ >>= _ = unreachable

-- }}}

-- To/From Vec {{{

fromVec :: Vec.Vec n a -> List (Repeat n a)
fromVec as = case as of
  Vec.Nil        -> Nil
  Vec.Cons a as' -> Cons a $ fromVec as'

{-
toVec :: forall as a. Same as a => List as -> Vec.Vec (Length as) a
toVec as = case as of
  Nil -> Vec.Nil
  Cons a as' -> case (sameEvidence :: SameEvidence as a) of
    SameCons p _ -> _
-}

{-
case  of
  SameNil         -> Vec.Nil
  SameCons Refl s -> case as of
    Cons a as' -> _
  -- case as of
  -- Nil        -> Vec.Nil
  -- Cons a as' ->   -- Vec.Cons a $ toVec as'
-}

{-
data SameEvidence (as :: [k]) (a :: k) where
  SameNil  :: SameEvidence '[] a
  SameCons :: a :~: b -> SameEvidence as a -> SameEvidence (b ': as) a

class Same (as :: [k]) (a :: k) where
  sameEvidence :: SameEvidence as a
instance Same '[] a where
  sameEvidence = SameNil
instance (a ~ b, Same as a) => Same (b ': as) a where
  sameEvidence = SameCons Refl sameEvidence
-}

-- }}}

-- Index {{{

mapI :: Index i as bs a b => Nat i -> (a -> b) -> List as -> List bs
mapI i f = runIdentity . _Index i (Identity . f)

(!!) :: forall i as bs a b. Index i as as a a => List as -> Nat i -> a
as !! i = getConstant $ (_Index i Constant as :: Constant a (List as))

setI :: Index i as bs a b => Nat i -> b -> List as -> List bs
setI i = mapI i . const

class Index (i :: N) (as :: [*]) (bs :: [*]) (a :: *) (b :: *)
  | i as -> a, i bs -> b, i as b -> bs, i bs a -> as where
  _Index :: Functor f => Nat i -> (a -> f b) -> List as -> f (List bs)

instance Index Z (a ': as) (b ': as) a b where
  _Index Z_ f (Cons a as) = fmap (($ as) . Cons) $ f a

instance Index x as bs a b => Index (S x) (c ': as) (c ': bs) a b where
  _Index (S_ x) f (Cons c as) = fmap (Cons c) $ _Index x f as

-- }}}

