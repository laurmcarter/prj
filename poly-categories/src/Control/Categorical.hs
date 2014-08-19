{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Control.Categorical where

import Prelude hiding (id,(.))
import qualified Prelude
import Data.Semigroupoid

class Semigroupoid cat => Category (cat :: k -> k -> *) where
  id :: cat a a

instance Category (->) where
  id = Prelude.id

