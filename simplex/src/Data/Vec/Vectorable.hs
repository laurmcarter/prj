{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Data.Vec.Vectorable where


import Data.Vec
import Data.Nat.Unary

import Control.Lens
import Linear
import Diagrams.Core.Points
import qualified Diagrams.TwoD.Types as D
import qualified Diagrams.ThreeD.Types as D

class Vectorable n v a | v -> n, v -> a where
  _Vec :: Iso' v (Vec n a)

instance Vectorable n (Vec n a) a where
  _Vec = id

instance Vectorable Zero (V0 a) a where
  _Vec = iso (const Nil) (const V0)
instance Vectorable One (V1 a) a where
  _Vec = iso toV fromV
    where
    toV (V1 a) = singleton a
    fromV (a :* _) = V1 a
instance Vectorable Two (V2 a) a where
  _Vec = iso toV fromV
    where
    toV (V2 a b) = a *: b
    fromV (a :* b :* _) = V2 a b
instance Vectorable Three (V3 a) a where
  _Vec = iso toV fromV
    where
    toV (V3 a b c) = a :* b *: c
    fromV (a :* b :* c :* _) = V3 a b c
instance Vectorable Four (V4 a) a where
  _Vec = iso toV fromV
    where
    toV (V4 a b c d) = a :* b :* c *: d
    fromV (a :* b :* c :* d :* _) = V4 a b c d

instance Vectorable Two D.R2 Double where
  _Vec = iso toV fromV
    where
    toV (D.R2 a b) = a *: b
    fromV (a :* b :* _) = D.R2 a b
instance Vectorable Three D.R3 Double where
  _Vec = iso toV fromV
    where
    toV (D.unr3 -> (a,b,c)) = a :* b *: c
    fromV (a :* b :* c :* _) = D.r3 (a,b,c)

instance Vectorable n v a => Vectorable n (Point v) a where
  _Vec = iso (\(P v) -> v) P . _Vec

