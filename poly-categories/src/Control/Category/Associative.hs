{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Category.Associative where

import qualified Prelude
import Prelude hiding (id,(.))

import Control.Categorical.Bifunctor
import Control.Category.Dual

class Bifunctor p k k k
  => Associative (k :: x -> x -> *) (p :: x -> x -> x) where
  associate    :: p (p a b) c `k` p a (p b c)
  disassociate :: p a (p b c) `k` p (p a b) c

instance Associative (->) (,) where
  associate    ((a,b),c) = (a,(b,c))
  disassociate (a,(b,c)) = ((a,b),c)

instance Associative (->) Either where
  associate = \case
    Left (Left  a) -> Left         a
    Left (Right b) -> Right (Left  b)
    Right       c  -> Right (Right c)
  disassociate = \case
    Left         a  -> Left (Left  a)
    Right (Left  b) -> Left (Right b)
    Right (Right c) -> Right       c 

instance Associative k p => Associative (Dual k) p where
  associate    = Dual disassociate
  disassociate = Dual associate

