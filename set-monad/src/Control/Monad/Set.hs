{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Monad.Set where

import Data.Set (Set)
import qualified Data.Set as S
import GHC.Exts (Constraint)

data Dict (c :: Constraint) where
  Dict :: c => Dict c

data SetM c a = SetM
  { setM :: Dict c -> Dict (Ord a) -> Set a
  }

