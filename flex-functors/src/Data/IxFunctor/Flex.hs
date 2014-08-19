{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.IxFunctor.Flex where

import GHC.Exts (Constraint)

type Flex f i j k l = (Joint f i j k l, JointL f i k, JointR f j l)

class Flexor (f :: ix -> ix -> * -> *) where
  type Joint f  :: ix -> ix -> ix -> ix -> Constraint
  type JointL f ::             ix -> ix -> Constraint
  type JointR f ::             ix -> ix -> Constraint
  flexL :: JointL f i j  => f i k a -> f j k a
  flexR :: JointR f i j  => f k i a -> f k j a
  flex  :: Flex f i j k l => f i j a -> f k l a

type Rfl
  (c :: (ix -> ix -> * -> *) -> ix -> ix -> k)
  (f ::  ix -> ix -> * -> *)
  (i ::  ix)
  = (c f i i :: k)

type Reflex 
  (c :: (ix -> ix -> * -> *) -> ix -> ix -> ix -> ix -> k)
  (f ::  ix -> ix -> * -> *)
  (i ::  ix)
  (j ::  ix)
  = (c f i j i j :: k)

