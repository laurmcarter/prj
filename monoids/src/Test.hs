{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Test where

import Prelude hiding (gcd, null)

import Data.Monoid
import Data.Monoid.Cancellative
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

s1 :: Set Int
s1 = S.fromList [0..9]

s2 :: Set Int
s2 = S.fromList [5..14]

m1 :: Map Int Int
m1 = M.fromSet id s1

m2 :: Map Int Int
m2 = M.fromSet id s2

-- Other Map instances {{{

instance Ord k => RightReductiveMonoid (Map k a) where
  isSuffixOf = M.isSubmapOfBy (\_ _-> True)
  stripSuffix a b | M.isSubmapOfBy (\_ _-> True) a b = Just (b M.\\ a)
                  | otherwise = Nothing

instance (Ord k, Eq a) => RightGCDMonoid (Map k a) where
  commonSuffix = M.mergeWithKey (\k a b -> if a == b then Just a else Nothing) (const M.empty) (const M.empty)

instance (Ord k, Eq a) => GCDMonoid (Map k a) where
  gcd = M.intersection

instance (Ord k, Eq a) => CommutativeMonoid (Map k a) where

instance (Ord k, Eq a) => ReductiveMonoid (Map k a) where
   a </> b | M.isSubmapOf b a = Just (a M.\\ b)
           | otherwise = Nothing

-- }}}

-- Subtractive Monoid {{{

class (GCDMonoid m, ReductiveMonoid m) => SubtractiveMonoid m where
  (<->) :: m -> m -> m

instance (GCDMonoid m, ReductiveMonoid m) => SubtractiveMonoid m where
  a <-> b = case a </> gcd a b of
    Nothing -> error "SubtractiveMonoid: bug in </> definition"
    Just c  -> c

-- }}}

