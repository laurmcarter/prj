{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , MultiParamTypeClasses
           , ViewPatterns
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Shapes
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Various three-dimensional shapes.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Shapes
       (
         Ellipsoid(..)
       , sphere
       ) where

import Prelude hiding (minimum)
import Data.Semigroup

import Data.AffineSpace
import Data.Monoid.Inf (PosInf,minimum)
import Data.VectorSpace

import Diagrams.Core

import Diagrams.ThreeD.Types
import Diagrams.Solve

import Control.Lens (allOf,each)

data Ellipsoid = Ellipsoid T3

type instance V Ellipsoid = R3

instance Transformable Ellipsoid where
  transform t1 (Ellipsoid t2) = Ellipsoid (t1 <> t2)

instance IsPrim Ellipsoid

instance Renderable Ellipsoid NullBackend where
  render _ _ = mempty

sphere :: (Backend b R3, Renderable Ellipsoid b) => Diagram b R3
sphere = mkQD (Prim $ Ellipsoid mempty)
              (mkEnvelope sphereEnv)
              (mkTrace sphereTrace)
              mempty
              (Query sphereQuery)
  where sphereEnv v = 1 / magnitude v
        sphereTrace p v = minimum (quadForm a b c)
          where a = magnitudeSq v
                b = 2 *^ p' <.> v
                c = magnitudeSq p' - 1
                p' = p .-. origin
        sphereQuery v = Any $ magnitudeSq (v .-. origin) <= 1

data Cuboid = Cuboid T3

type instance V Cuboid = R3

instance Transformable Cuboid where
  transform t1 (Cuboid t2) = Cuboid (t1 <> t2)

instance IsPrim Cuboid

instance Renderable Cuboid NullBackend where
  render _ _ = mempty

cube :: (Backend b R3, Renderable Cuboid b) => Diagram b R3
cube = mkQD (Prim $ Cuboid mempty) -- Prim b R3
            (mkEnvelope cubeEnv)   -- Envelope R3
            (mkTrace cubeTrace)    -- Trace R3
            mempty                 -- SubMap b R3 Any
            (Query cubeQuery)      -- Query R3 Any
  where
  -- all assuming unit cube
  --
  -- distance to bounding plane
  cubeEnv :: R3 -> Double
  cubeEnv v = undefined
  -- from point and direction, distance to intersection
  cubeTrace :: P3 -> R3 -> PosInf Double
  cubeTrace p v = undefined
  -- is point in cube?
  cubeQuery :: P3 -> Any
  cubeQuery = Any . allOf each ((<= (1/2)) . abs) . unp3

