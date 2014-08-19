{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Constraint.Recover where

import Data.Constraint
import Data.Constraint.Unsafe

class Recover (c :: k -> Constraint) (a :: k) where
  type Recovery c a :: Constraint
  recover :: c a :- Recovery c a

instance Recover Show (a,b) where
  type Recovery Show (a,b) = (Show a, Show b)
  recover = unsafeCoerceConstraint 

