
module Sparse where

import Prelude hiding (elem,notElem)

import qualified Numeric.Interval as I
import Data.Set         (Set(..))
import qualified Data.Set         as S



newtype Sparse a = Sparse { fromSparse :: S.Set (I.Interval a) }
  deriving (Eq,Ord,Show)

fromList :: Ord a => [(a,a)] -> Sparse a
fromList = Sparse . S.fromList . map (uncurry I.I)

elem :: Ord a => a -> Sparse a -> Bool
elem a = anySet (I.elem a) . fromSparse

notElem :: Ord a => a -> Sparse a -> Bool
notElem a = not . elem a

-- Set Helpers

anySet :: (a -> Bool) -> S.Set a -> Bool
anySet f = any f . S.toList

allSet :: (a -> Bool) -> S.Set a -> Bool
allSet f = all f . S.toList

