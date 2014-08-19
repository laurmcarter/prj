{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Fin where

import Data.Nat

data Fin (n :: N) where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)

fin :: Fin n -> Int
fin n = case n of
  FZ    -> 0
  FS n' -> succ $ fin n'

weaken1 :: Fin n -> Fin (S n)
weaken1 n = case n of
  FZ    -> FZ
  FS n' -> FS $ weaken1 n'

widen1 :: Fin n -> Fin (S n)
widen1 n = case n of
  FZ    -> FS FZ
  FS n' -> FS $ widen1 n'

{-
tighten1 :: Fin (S n) -> Maybe (Fin n)
tighten1 
-}

