{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Control.Category.Dual where

import Data.Semigroupoid   (Semigroupoid)
import qualified Data.Semigroupoid   as S
import Control.Categorical (Category)
import qualified Control.Categorical as C

data Dual :: (k -> k -> *) -> k -> k -> * where
  Dual :: { runDual :: p b a } -> Dual p a b

instance Semigroupoid sem => Semigroupoid (Dual sem) where
  Dual cb . Dual ba = Dual $ ba S.. cb

instance Category cat => Category (Dual cat) where
  id = Dual C.id

dualInv :: Dual (Dual k) a b -> k a b
dualInv = runDual . runDual

