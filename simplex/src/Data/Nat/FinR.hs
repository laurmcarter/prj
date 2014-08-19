{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data.Nat.FinR where

import Data.Vec.Unary

import Prelude
import qualified Prelude as P
import Data.Proxy

data Fin (n :: UN) where
  FZ :: Nat n => Fin (S n)
  FS :: Nat n => Fin n -> Fin (S n)

instance AsProxy (Fin n) n

fin :: (Num a, Enum a) => Fin n -> a
fin n = case n of
  FZ -> 0
  FS n' -> succ $ fin n'

instance Nat n => Show (Fin n) where
  showsPrec d n = showParen (d > 5)
    $ showString "Fin "
    . shows (nat $ asProxy n :: Int)
    . showString " "
    . shows (fin n :: Int)

showFin :: Nat n => Fin n -> String
showFin = flip (showsPrecFin 0) ""

showsPrecFin :: Nat n => Int -> Fin n -> ShowS
showsPrecFin d n = case n of
  FZ    -> showString "FZ"
  FS n' -> showParen (d > 5)
    $ showString "FS "
    . showsPrecFin 11 n'

weaken1 :: Fin n -> Fin (S n)
weaken1 n = case n of
  FZ -> FZ
  FS n' -> FS $ weaken1 n'

widen1 :: Fin n -> Fin (S n)
widen1 n = case n of
  FZ -> FS FZ
  FS n' -> FS $ widen1 n'

class Nat n => Finite n where
  tighten1 :: Fin (S n) -> Maybe (Fin n)

instance Finite Z where
  tighten1 _ = Nothing

instance Finite n => Finite (S n) where
  tighten1 n = case n of
    FZ -> Just FZ
    FS n' -> undefined

{-

tighten1 :: FinR (S n) -> Maybe (FinR n)
tighten1 n = case n of
  FN    -> Nothing
  FP n' -> Just n'
-}

fin1 :: Fin One
fin1 = FZ

fin2 :: Fin Two
fin2 = FZ

fin3 :: Fin Three
fin3 = FZ

fin4 :: Fin Four
fin4 = FZ

