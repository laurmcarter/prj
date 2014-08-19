{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Category.Braided where

import Control.Category.Associative
import Control.Category.Dual

class Associative k p => Braided (k :: x -> x -> *) (p :: x -> x -> x) where
  braid :: p a b `k` p b a

instance Braided (->) Either where
  braid = \case
    Left  a -> Right a
    Right b -> Left  b

instance Braided (->) (,) where
  braid ~(a,b) = (b,a)

class Braided k p => Symmetric (k :: x -> x -> *) (p :: x -> x -> x)

swap :: Symmetric k p => p a b `k` p b a
swap = braid

instance Symmetric (->) Either
instance Symmetric (->) (,)

instance Braided k p => Braided (Dual k) p where
  braid = Dual braid

instance Symmetric k p => Symmetric (Dual k) p

