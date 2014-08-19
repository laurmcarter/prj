{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Rank2Types #-}

module Data.Simplex where

import Data.Nat.Unary
import Data.Vec

import Control.Applicative
import Data.Proxy
import Data.Tagged
import Data.Void
import Linear

type Simplex' v a = Tagged v (Vec (SN v) a)

-- (SN v) of (v a)
type Simplex v a = Simplex' v (v a)
type SimplexIn v w a = Simplex' v (w a)

x :: SimplexIn V2 V3 Int
x = Tagged $ pure 0 :* pure 1 *: pure 2

type PrevSimplex' v a = Tagged (PrevDim v) (VN v a)
type PrevSimplex v a = Tagged (PrevDim v) (VN v (v a))
type SN v = (S (N v))
type VN v = Vec (N v)
type VSN v = Vec (S (N v))

class Simplicial (v :: * -> *) where
  type PrevDim v :: * -> *
  type N v :: UN
  type N v = SN (PrevDim v)
  embeddedSimplices ::
    ( Simplicial w
    , Combine (SN w)
    , NLE (N w) (N v)
    ) => Proxy w
      -> Simplex' v a
      -> Vec (NChoose (SN v) (SN w)) (Simplex' w a)
  embeddedSimplices (_ :: Proxy w) (unTagged -> v) = fmap Tagged
    $ combine (Proxy :: Proxy (SN w)) v

instance Simplicial V0 where
  type PrevDim V0 = Const Void
  type N V0 = Z

instance Simplicial V1 where
  type PrevDim V1 = V0

instance Simplicial V2 where
  type PrevDim V2 = V1

instance Simplicial V3 where
  type PrevDim V3 = V2

instance Simplicial V4 where
  type PrevDim V4 = V3

v0 :: Proxy V0
v0 = Proxy
v1 :: Proxy V1
v1 = Proxy
v2 :: Proxy V2
v2 = Proxy
v3 :: Proxy V3
v3 = Proxy
v4 :: Proxy V4
v4 = Proxy

