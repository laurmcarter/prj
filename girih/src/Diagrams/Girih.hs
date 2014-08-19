{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Diagrams.Girih where

import Prelude hiding (repeat)
import qualified Prelude
import Control.Lens
import Diagrams.Angle
import Diagrams.Core
import Diagrams.Trail
import Diagrams.Located
import Diagrams.TrailLike
import Diagrams.TwoD
import Diagrams.TwoD.Size
import Diagrams.TwoD.Shapes
import Diagrams.Util
import Diagrams.Backend.SVG
import Data.Monoid

data Tile p = Tile
  { tileVerts  :: [(p,p)]
  , tilePos    :: p
  , tileOrient :: Angle
  } deriving (Eq,Show)


type T = (TrailLike t, V t ~ R2) => t

gDec :: T
gDec = sides
  $ repeat 10
  $ internal [ 144 ]

gHex :: T
gHex = sides
  $ repeat 2
  $ internal [ 72 , 144 , 144 ]

gBowtie :: T
gBowtie = sides
  $ repeat 2
  $ internal [ 72 , 72 , 216 ]

gRhomb :: T
gRhomb = sides
  $ repeat 2
  $ internal [ 72 , 108 ]

gPent :: T
gPent = sides
  $ repeat 5
  $ internal [ 108 ]

sides :: [Angle] -> T
sides as = polygon $ with
  & polyType .~ PolySides as ss
  where
  ss = 1 : map (const 1) as

internal :: [Double] -> [Angle]
internal = map $ (^. from deg) . (180 -)

girih :: IO ()
girih = renderSVG "girih.svg"
  (Dims 512 512)
   $ gDec
  <> gHex
  <> gBowtie
  <> gRhomb
  <> gPent

repeat :: Int -> [a] -> [a]
repeat n = concat . replicate n

