{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}

module Data.GCL where

import Data.Constraint.Evidence
import Data.Constraint.Set
import GHC.Exts (Constraint)
import Control.Monad
import Data.Type.Equality

class Join (r1 :: * -> *) (r2 :: * -> *) (r3 :: * -> *)
  | r1 r2 -> r3 , r1 r3 -> r2 where
  (<&>) :: r1 () -> r2 a -> r3 a

class Repeat (r1 :: * -> *) (r2 :: * -> *) where
  rep :: r1 () -> r2 ()

class GCL (r :: * -> *) where
  -- lifting into guard
  grd   :: r Bool -> r a -> r a
  -- lowering back out
  alt   :: [r a] -> r a

class Vars (r :: N -> N -> [(N,Maybe *)] -> * -> *) where
  type family Var r :: N -> *
  declare :: (Var r x -> r (S x) y (Declare x env) a) -> r x y env a
  assign  :: Var r x -> typ -> r y z '[ '(x,Just typ) ] ()
  var     :: Assigned x typ env => Var r x -> r y z env typ

newtype Run (x :: N) (y :: N) env a = Run
  { run :: Env env -> Maybe a
  }

instance GCL (Run x y env) where
  grd g e = Run $ \env -> do
    b <- run g env
    if b
      then run e env
      else Nothing
  alt = Run . go
    where
    go :: [Run x y env a] -> Env env -> Maybe a
    go as env = case as of
      []      -> Nothing
      a : as' -> run a env `mplus` go as' env

instance Progress e1 e2 => Repeat (Run x x e1) (Run x x e2) where
  rep e = Run $ run e . regress

{-
instance (e3 ~ (e1 <> e2)) => Join (Run x y e1) (Run y z e2) (Run x z e3) where
  e1 <&> e2 = _
-}

{-
-- t0 :: GCL r => 
t0 :: GCL r =>
t0 = alt
  [ grd True
  $ declare $ \x -> _
  ]
-}



type family (x :: N) + (y :: N) :: N where
  Z   + y = y
  S x + y = S (x + y)

type family (as :: [(k,l)]) <> (bs :: [(k,l)]) :: [(k,l)] where
  '[]       <> bs = bs
  ('(x,y) ': as) <> bs = Insert x y (as <> bs)

(<>) :: Env e1 -> Env e2 -> Env (e1 <> e2)
e1 <> e2 = case e1 of
  Nil            -> e2
  Bound x mt e1' -> undefined

type family Insert (x :: k) (y :: l) (as :: [(k,l)]) :: [(k,l)] where
  Insert x y '[] = '[ '(x,y) ]
  Insert x y  ('(x,z) ': as) = '(x,y) ': as
  Insert x y  ('(a,b) ': as) = '(a,b) ': Insert x y as

class Bind (x :: N) (mt :: Maybe *) (e1 :: [(N,Maybe *)]) (e2 :: [(N,Maybe *)]) | x mt e1 -> e2 where
  bind :: Nat x -> Option mt -> Env e1 -> Env e2

instance Bind x Nothing '[] '[ '(x,Nothing) ] where
  bind = Bound

instance (NatEQ x y False,Bind x Nothing e1 e2) => Bind x Nothing ('(y,mt) ': e1) ('(y,mt) ': e2) where
  bind x mt (Bound y mu e) = Bound y mu $ bind x mt e

instance Bind x (Just t) '[] '[ '(x,Just t) ] where
  bind = Bound

class Bind' (r :: Bool) (x :: N) (mt :: Maybe *) (y :: N) (mu :: Maybe *) (e1 :: [(N,Maybe *)]) (e2 :: [(N,Maybe *)]) | r x mt e1 -> e2 where
  bind' :: Nat x -> Option mt -> Nat y -> Option mu -> Env e1 -> Env e2

instance Bind' r x Nothing '[] '[ '(x,Nothing) ] where
  bind' = Bound

instance (Bind' r x Nothing e1 e2) => Bind' x Nothing ('(y,mt) ': e1) ('(y,mt) ': e2) where
  bind' x mt (Bound y mu e) = Bound y mu $ bind' x mt e

instance Bind' x (Just t) '[] '[ '(x,Just t) ] where
  bind' = Bound

class NatEQ (x :: N) (y :: N) (r :: Bool) | x y -> r

natEq :: Nat x -> Nat y -> Maybe (x :~: y)
natEq x y = case (x,y) of
 (Z_,Z_)       -> Just Refl
 (S_ x',S_ y') -> fmap (apply Refl) $ natEq x' y'
 _             -> Nothing

data N
  = Z
  | S N
  deriving (Eq,Show)

data Nat (x :: N) where
  Z_ :: Nat Z
  S_ :: Nat n -> Nat (S n)

data Option (a :: Maybe *) where
  Nothing_ :: Option Nothing
  Just_    :: a -> Option (Just a)

data Env (env :: [(N,Maybe *)]) where
  Nil   :: Env '[]
  Bound :: Nat x -> Option mt -> Env env -> Env ('(x,mt) ': env)

type Declare x env = '(x,Nothing) ': env



class Progress (e1 :: [(N,Maybe *)]) (e2 :: [(N,Maybe *)]) where
  regress :: Env e2 -> Env e1

instance Progress '[] '[] where
  regress Nil = Nil

instance Progress e1 e2 => Progress ('(x,mt) ': e1) ('(x,mt) ': e2) where
  regress (Bound x mt e2) = Bound x mt $ regress e2

instance Progress e1 e2 => Progress ('(x,Nothing) ': e1) ('(x,Just t ) ': e2) where
  regress (Bound x _  e2) = Bound x Nothing_ $ regress e2



class Assigned (x :: N) (t :: *) (env :: [(N,Maybe *)])
instance (mt ~ Just t)    => Assigned x t ('(x,mt) ': env)
instance Assigned x t env => Assigned x t ('(y,mt) ': env)


class    HasType (x :: N) (v :: *) (env :: [(N,Maybe *)])
instance (MUnify mv v,Absent x e) => HasType x v ('(x,mv) ': e)
instance HasType x v e            => HasType x v ('(y,w)  ': e)

class MUnify (m :: Maybe *) (a :: *)
instance            MUnify Nothing  a
instance (a ~ b) => MUnify (Just a) b

type Absent x env = Occurs x env False

class    Occurs (x :: N) (env :: [(N,Maybe *)]) (r :: Bool) | x env -> r
instance                   Occurs x '[] False
instance                   Occurs x ('(x,t) ': env) True
instance Occurs x env r => Occurs x ('(y,w) ': env) r

newtype Compose f g a = Compose
  { getCompose :: f (g a)
  }

