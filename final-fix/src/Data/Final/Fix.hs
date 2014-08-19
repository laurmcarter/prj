{-# LANGUAGE KindSignatures #-}

module Data.Final.Fix where

class Bind (r :: * -> *) where
  lam :: (r a -> r b) -> r (a -> b)
  fix :: (r a -> r a) -> r a



