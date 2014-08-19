
module Linear.Test where

import Linear.V2
import Linear.Affine
import Linear.Vector
import qualified Data.Map as M

type Coord = V2
type Size  = V2


-- Grid {{{

{-
newtype Grid a = Grid
  { fromGrid :: M.Map Coord a
  } deriving (Eq,Show,Functor,F.Foldable,T.Traversable)

mkEmptyGrid :: Size -> a -> Grid a
mkEmptyGrid sz a = Grid $ M.fromList
  [ (Coord { row = r , col = c },a)
  | c <- [0..width  sz - 1]
  , r <- [0..height sz - 1]
  ]

gridSubMap :: Coords -> Grid a -> Grid a
gridSubMap cs = gridOnMap $ M.filterWithKey fn
  where
  fn c _ = coordIsIn cs c

gridSubMapByValue :: Eq a => a -> Grid a -> Grid a
gridSubMapByValue = gridOnMap . M.filter . (==)

gridFromList :: [(Coord,a)] -> Grid a
gridFromList = Grid . M.fromList

gridSize :: Grid a -> Size
gridSize =
    uncurry (-)
  . onPair fromWidthHeight
  . (onPair maximum &&& onPair minimum)
  . colsRows
  . M.keys
  . fromGrid

gridRows :: Grid a -> Int
gridRows = height . gridSize

gridCols :: Grid a -> Int
gridCols = width . gridSize

gridCoords :: Grid a -> [Coord]
gridCoords = M.keys . fromGrid

gridFilter :: (a -> Bool) -> Grid a -> Grid a
gridFilter = gridOnMap . M.filter

gridMember :: Coord -> Grid a -> Bool
gridMember c = M.member c . fromGrid

gridIndex :: Grid a -> Coord -> a
-}
