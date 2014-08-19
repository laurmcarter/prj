{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module LambdaPi.Term where

import Data.Proxy
import GHC.TypeLits (Symbol)

data N
  = Z
  | S N
  deriving (Eq,Show)

data Nat (x :: N) where
  Z_ :: Nat Z
  S_ :: Nat n -> Nat (S n)

data Term
  = Ann Term Term
  | Star N
  | Lam Term Term Term
  | B N
  | F Name
  | Pi Term Term Term
  | App Term Term

data Term_ (a :: Term) where
  Ann_  :: Term_ e -> Term_ t        -> Term_ (Ann e t)
  Star_ :: Nat   n -> Term_ (Star n)
  Lam_  :: Term_ a -> Term_ b        -> Term_ (Lam x a b)
  B_    :: Nat   n -> Term_ (B n)
  F_    :: Name_ x -> Term_ (F x)
  Pi_   :: Term_ t -> Term_ t'       -> Term_ (Pi x t t')
  App_  :: Term_ f -> Term_ x        -> Term_ (App f x)

data Name
  = Local  N
  | Quoted N
  | forall a. Global a

data Name_ (x :: Name) where
  Local_  :: Nat n               -> Name_ (Local  n)
  Quoted_ :: Nat n               -> Name_ (Quoted n)
  Global_ :: Proxy (x :: Symbol) -> Name_ (Global x)

