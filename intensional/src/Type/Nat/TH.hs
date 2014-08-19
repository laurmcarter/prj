{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Type.Nat.TH where

import Type.Nat

import Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH as TH

nat :: Int -> Q TH.Type
nat n
  | n < 0     = fail "Negative posing as Natural"
  | n == 0    = [t| Z |]
  | otherwise = [t| S $(nat (n-1)) |]

