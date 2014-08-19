{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Test where

import TH

import qualified Graphics.Rendering.Cairo as Cairo

import Control.Applicative
import Control.Lens
import Data.Foldable (Foldable(..))
import Linear

-- Helpers {{{

r1 :: (R1 f) => (a -> r) -> f a -> r
r1 f r = f (r^._x)

r2 :: (R2 f) => (a -> a -> r) -> f a -> r
r2 f r = f (r^._x) (r^._y)

r3 :: (R3 f) => (a -> a -> a -> r) -> f a -> r
r3 f r = f (r^._x) (r^._y) (r^._z)

r4 :: (R4 f) => (a -> a -> a -> a -> r) -> f a -> r
r4 f r = f (r^._x) (r^._y) (r^._z) (r^._w)



(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(.::) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.::) = (.:) . (.:)

v2 :: (V2 a -> r) -> a -> a -> r
v2 f = f .: V2

v4 :: (V4 a -> r) -> a -> a -> a -> a -> r
v4 f = f .:: V4

-- }}}

-- Types {{{

newtype Coord' a = Coord'
  { _coordV2 :: V2 a
  } deriving
    ( Eq, Show, Num
    , Fractional
    , Epsilon, R1, R2, Additive
    , Functor, Applicative, Monad
    , Foldable, Traversable
    )
makeIso' ''Coord'
mkCoord :: a -> a -> Coord' a
mkCoord = v2 Coord'

newtype Size' a = Size'
  { _sizeV2 :: V2 a
  } deriving
    ( Eq, Show, Num
    , Fractional
    , Epsilon, R1, R2, Additive
    , Functor, Applicative, Monad
    , Foldable, Traversable
    )
makeIso' ''Size'
mkSize :: a -> a -> Size' a
mkSize = v2 Size'

newtype Canvas' a = Canvas'
  { _canvasV2 :: V2 a
  } deriving
    ( Eq, Show, Num
    , Fractional
    , Epsilon, R1, R2, Additive
    , Functor, Applicative, Monad
    , Foldable, Traversable
    )
makeIso' ''Canvas'
mkCanvas :: a -> a -> Canvas' a
mkCanvas = v2 Canvas'

data Rect' a = Rect'
  { _rectCoord :: Coord' a
  , _rectSize  :: Size' a
  } deriving (Eq,Show)
makeLenses ''Rect'
mkRect :: a -> a -> a -> a -> Rect' a
mkRect x y w h = Rect' (mkCoord x y) (mkSize w h)

instance Field1 (Rect' a) (Rect' a) (Coord' a) (Coord' a) where
  _1 = indexing rectCoord

instance Field2 (Rect' a) (Rect' a) (Size' a) (Size' a) where
  _2 = indexing rectSize

newtype RGBA' a = RGBA'
  { _rgbaV4 :: V4 a
  } deriving
    ( Eq, Show, Num
    , Fractional
    , Epsilon, R1, R2, R3, R4, Additive
    , Functor, Applicative, Monad
    , Foldable, Traversable
    )
makeIso' ''RGBA'
mkRGBA :: a -> a -> a -> a -> RGBA' a
mkRGBA = v4 RGBA'

type Coord  = Coord'  Double
type Size   = Size'   Double
type Canvas = Canvas' Int
type Rect   = Rect'   Double
type RGBA   = RGBA'   Double

-- }}}

drawColoredRectAtPoint :: Canvas -> Rect -> RGBA -> RGBA -> Cairo.Operator -> Cairo.Render ()
drawColoredRectAtPoint canv rect bg fg rator = do
  Cairo.renderWithSimilarSurface Cairo.ContentColorAlpha `r2` canv $ \s -> do
    Cairo.setSourceSurface s `r2` origin
    Cairo.renderWith s $ do
      Cairo.setSourceRGBA `r4` bg
      Cairo.rectangle `r2` (rect^.rectCoord) `r2` (rect^.rectSize)
      Cairo.fillPreserve
      Cairo.setSourceRGBA `r4` fg
      Cairo.stroke
    Cairo.setOperator rator
    Cairo.maskSurface s `r2` origin
  where
  origin = 0 :: Coord

