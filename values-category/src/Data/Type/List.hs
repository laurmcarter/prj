{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Type.List where

import Prelude hiding ((++),head,tail,init,last,null,length)
import qualified Prelude as P
import Data.Proxy
import GHC.Exts (Constraint)
import Data.Typeable

import Data.Type.Bool
import Data.Type.Equality
-- import Data.Type.Focus
-- import Data.Type.Focus.TH

-- Type Witness {{{

data List (as :: [*]) where
  Nil  :: List '[]
  Cons :: a -> List as -> List (a ': as)

data B (x :: Bool) where
  True_  :: B True
  False_ :: B False

(&&:) :: B x -> B y -> B (x && y)
x &&: y = case x of
  True_  -> y
  False_ -> False_
infixr 3 &&:

(||:) :: B x -> B y -> B (x || y)
x ||: y = case x of
  True_  -> True_
  False_ -> y
infixr 2 ||:

data Guarded (p :: Bool) (a :: *) where
  Fail   ::            Guarded False a
  Succ   :: a       -> Guarded True a
  Unsure :: (p ~ True => Maybe a) -> Guarded p a

data N
  = Z
  | S N
  deriving (Eq,Show)

data Nat (x :: N) where
  Z_ :: Nat Z
  S_ :: Nat x -> Nat (S x)

type instance x == y = NatEq x y
type family NatEq (x :: N) (y :: N) :: Bool where
  NatEq  Z     Z    = True
  NatEq  Z    (S y) = False
  NatEq (S x)  Z    = False
  NatEq (S x) (S y) = NatEq x y

-- }}}

-- Type Families {{{

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)
infixr 5 ++

type family Member (a :: k) (as :: [k]) :: Bool where
  Member a '[]       = False
  Member a (b ': as) = a == b || Member a as

type family Head (as :: [k]) :: k where
  Head (a ': as) = a

type family Tail (as :: [k]) :: [k] where
  Tail (a ': as) = as

type family Init (as :: [k]) :: [k] where
  Init (a ': as) = Init' a as

type family Init' (a :: k) (as :: [k]) :: [k] where
  Init' a '[]       = '[]
  Init' a (b ': as) = a ': Init' b as

type family Last (as :: [k]) :: k where
  Last (a ': as) = Last' a as

type family Last' (a :: k) (as :: [k]) :: k where
  Last' a '[]       = a
  Last' a (b ': as) = Last' b as

type family Null (as :: [k]) :: Bool where
  Null '[]       = True
  Null (a ': as) = False

type family Length (as :: [k]) :: N where
  Length '[]       = Z
  Length (a ': as) = S (Length as)

type family Snoc (as :: [k]) (a :: k) :: [k] where
  Snoc '[]       a = '[a]
  Snoc (b ': as) a = b ': Snoc as a

type family Reverse (as :: [k]) :: [k] where
  Reverse '[]       = '[]
  Reverse (a ': as) = Snoc (Reverse as) a

type family Intersperse (s :: k) (as :: [k]) :: [k] where
  Intersperse s '[]       = '[]
  Intersperse s (a ': as) = a ': s ': Intersperse s as

type family Intercalate (s :: [k]) (as :: [[k]]) :: [k] where
  Intercalate s '[]       = '[]
  Intercalate s (a ': as) = a ++ s ++ Intercalate s as

type family StripPrefix (as :: [k]) (bs :: [k]) :: [k] where
  StripPrefix '[]        bs       = bs
  StripPrefix (a ': as) (b ': bs) = StripPrefix as bs

type family IsPrefix (as :: [k]) (bs :: [k]) :: Bool where
  IsPrefix '[] bs              = True
  IsPrefix (a ': as) '[]       = False
  IsPrefix (a ': as) (b ': bs) = (a == b) && IsPrefix as bs

{-
type family Map (f :: *) (xs :: [k]) :: [*] where
  Map f '[]       = '[]
  Map f (x ': xs) = Clos f x ': Map f xs
-}

{-
type family Transpose (as :: [[k]]) :: [[k]] where
type family Transpose (as :: [[k]]) :: [[k]] where
  Transpose '[]               = '[]
  Transpose ('[] ': as)       = Transpose as
  Transpose ((x ': xs) ': as) = 
-}

-- }}}

-- List Ops {{{

(++) :: List as -> List bs -> List (as ++ bs)
as ++ bs = case as of
  Nil        -> bs
  Cons a as' -> Cons a $ as' ++ bs

head :: List (a ': as) -> a
head (Cons a _) = a

tail :: List (a ': as) -> List as
tail (Cons _ as) = as

init :: List (a ': as) -> List (Init (a ': as))
init (Cons a as) = init' a as
  where
  init' :: a -> List as -> List (Init' a as)
  init' a as = case as of
    Nil         -> Nil
    Cons a' as' -> Cons a $ init' a' as'

last :: List (a ': as) -> Last (a ': as)
last (Cons a as) = last' a as
  where
  last' :: a -> List as -> Last' a as
  last' a as = case as of
    Nil         -> a
    Cons a' as' -> last' a' as'

null :: List as -> B (Null as)
null as = case as of
  Nil     -> True_
  Cons {} -> False_

length :: List as -> Nat (Length as)
length as = case as of
  Nil        -> Z_
  Cons _ as' -> S_ $ length as'

{-
isPrefix :: (All Typeable as, All Typeable bs) => List as -> List bs -> B (IsPrefix as bs)
isPrefix as bs = case (as,bs) of
  (Nil,_)                 -> True_
  (Cons (a :: a) as',Cons (b :: b) bs')
    -> (undefined :: B (a == b)) &&: isPrefix as' bs'
  (Cons _ _,Nil) -> False_
-}

data Constrained (c :: k -> Constraint) (f :: k -> *) (a :: k) = Constrained
  { constraint  :: Dict (c a)
  , constrained :: f a
  }

data Dict (c :: Constraint) where
  Dict :: c => Dict c

instance TestEquality (Constrained Typeable f) where
  testEquality a b = case (constraint a, constraint b) of
    (Dict,Dict) -> eqT

{-
stripPrefix :: List as -> List bs -> Guarded (IsPrefix as bs) (List (StripPrefix as bs))
stripPrefix as bs = case as of
  Nil        -> Just_ bs
  Cons a as' -> case bs of
    Nil        -> Nothing_
    Cons b bs' -> _
-}

-- }}}

type family Guard (p :: Bool) (a :: k) :: Maybe k where
  Guard True  a = Just a
  Guard False a = Nothing

guard :: B p -> a -> Guarded p a
guard p a = case p of
  True_  -> Succ a
  False_ -> Fail

instance TestEquality Nat where
  testEquality a b = case (a,b) of
    (Z_,Z_)       -> qed
    (S_ a',S_ b') -> testEquality a' b' ==> qed
    (Z_,S_ b')    -> Nothing
    (S_ a',Z_)    -> Nothing

(==>) :: Maybe (a :~: b) -> (a ~ b => Maybe r) -> Maybe r
mp ==> r = case mp of
  Just Refl -> r
  Nothing   -> Nothing
infixr 4 ==>

(?=>) :: Guarded p (a :~: b)
  -> ((p ~ True, a ~ b) => r)
  -> Guarded p r
gd ?=> r = case gd of
  Succ Refl -> Succ r
  Fail      -> Fail
  Unsure mp -> Unsure $ case mp of
    Just Refl -> Just r
    _         -> Nothing

qed :: Maybe (c :~: c)
qed = Just Refl

gQed :: Guarded True (c :~: c)
gQed = Succ Refl

class TestBEQ (f :: k -> *) where
  testBEQ :: f a -> f b -> B (a == b)

instance TestBEQ Nat where
  testBEQ a b = case (a,b) of
    (Z_   ,Z_   ) -> True_
    (Z_   ,S_ b') -> False_
    (S_ a',Z_   ) -> False_
    (S_ a',S_ b') -> testBEQ a' b'

class TestEquality' (f :: k -> *) where
  testEquality' :: f a -> f b -> Guarded (a == b) (a :~: b)

instance TestEquality' Nat where
  testEquality' (a :: Nat a) (b :: Nat b) = case (a,b) of
    (Z_,Z_)       -> gQed
    (S_ a',S_ b') -> testEquality' a' b' ?=> Refl
    (Z_,S_ b')    -> Fail
    (S_ a',Z_)    -> Fail

instance TestEquality' ((:~:) a) where
  testEquality' (p :: a :~: b) (q :: a :~: c) = case testEquality p q of
    Just Refl -> Unsure $ Just Refl
    _         -> Unsure $ Just $ sym p `trans` q

if_ :: B t -> a -> a -> a
if_ t c a = case t of
  True_  -> c
  False_ -> a

ifTF :: B x -> (If x True False) :~: x
ifTF x = case x of
  True_  -> Refl
  False_ -> Refl

ifAnd :: B x -> B y -> (If x y False) :~: (x && y)
ifAnd x y = case x of
  True_  -> Refl
  False_ -> Refl

ifOr :: B x -> B y -> (If x True y) :~: (x || y)
ifOr x y = case x of
  True_  -> Refl
  False_ -> Refl

ifBranch :: B x -> B y -> B z -> (If x y z) :~: ((x && y) || (Not x && z))
ifBranch x y z = case x of
  True_  -> Refl
  False_ -> Refl

{-
-- Symbol application {{{
data L_Append
data L_Member
data L_Head
data L_Tail
data L_Init
data L_Init'
data L_Last
data L_Last'
data L_Null
data L_Length
data L_Snoc
data L_Reverse
data L_Intersperse
data L_Intercalate
data L_Map
data Clos (f :: *) (x :: k)
data L_Apply
data L_Flip

type family Arity (f :: *) :: N
type family ApplyArity (f :: *) (n :: N) (x :: k) :: l where
  -- Saturated
  ApplyArity (Clos f y)    (S Z)  x = Apply f '(y,x)
    -- apply closure
  ApplyArity f             (S Z)  x = Apply f x
    -- apply directly to forehead
  -- Undersaturated
  ApplyArity (Clos f y) (S (S n)) x = Clos f '(y,x)
    -- extend existing closure
  ApplyArity f          (S (S n)) x = Clos f x
    -- build initial closure

data Clos (f :: *) (x :: k)
-- type instance (Clos f x) :@ () = Apply f x

ex1 = Proxy :: Proxy (Arity L_Apply)
-- S (S Z)
ex2 = Proxy :: Proxy (Arity (Clos L_Apply L_Member))
-- S Z
ex3 = Proxy :: Proxy (Arity (Clos L_Apply '(L_Member,'[()])))
-- Z
ex4a = Proxy :: Proxy (Arity L_Member)
ex4  = Proxy :: Proxy (L_Member :@ Int)

type instance Arity (Clos f x) = ArityMinus (Arity f) x

type family ArityMinus (n :: N) (x :: k) :: N where
  ArityMinus (S n) '(a,b) = ArityMinus n b
  ArityMinus (S n) a      = n

data L_Apply
type instance Arity L_Apply = S (S Z)

type family (f :: *) :@ (x :: k) :: l where
  f :@ x = ApplyArity f (Arity f) x
-- type instance Apply L_Apply '(f,x) = Apply f x
infixl 4 :@

type family Apply (f :: *) (x :: k) :: l

data L_Flip
-- type instance Apply L_Flip p = Flip p
type family Flip (p :: (k,l)) :: (l,k) where
  Flip '(a,b) = '(b,a)
-- }}}
-}

