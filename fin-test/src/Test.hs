
module Test where

import Prelude hiding (Enum(..))
import Prelude.SafeEnum

import Data.Maybe (fromJust)
import Data.Number.Fin
import Data.Number.Fin.TyDecimal hiding (succ,pred)

complement :: Nat n => Fin n -> Fin n
complement n = case pred n of
  Just n' -> case pred $ complement n' of
    Just n'' -> n''
    _        -> error "Wut"
  _       -> maxBound

{-

0 , 1 , ... , n-2 , n-1
0 , 1 , ... , n-2 , n-1

-}

