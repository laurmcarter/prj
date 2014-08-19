{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Data.Monoid.Nests where

import Data.Monoid.Splits (Splits(..))
import qualified Data.Monoid.Splits as S

import Control.Applicative
import Control.Lens
import Data.Monoid
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as SG
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T

data Nests b a
  = Unnested a
  | Nested   a b (Nests b a) (Nests b a)
  deriving (Eq,Show)

makePrisms ''Nests

instance Functor (Nests b) where
  fmap f = go
    where
    go = \case
      Unnested a  -> Unnested $ f a
      Nested a b
        (go -> l)
        (go -> r) -> Nested (f a) b l r

instance Foldable (Nests b) where
  foldMap f = go
    where
    go = \case
      Unnested a  -> f a
      Nested a b
        (go -> l)
        (go -> r) -> f a <> l <> r

instance Traversable (Nests b) where
  traverse f = go
    where
    go = \case
      Unnested a  -> Unnested <$> f a
      Nested a b
        (go -> l)
        (go -> r) -> Nested <$> f a <*> pure b <*> l <*> r

instance Monoid a => Monoid (Nests b a) where
  mempty  = Unnested mempty
  mappend = curry $ \case
    (Unnested a     , Unnested a'       ) -> Unnested (a <> a')
    (Unnested a     , Nested   a' b l r ) -> Nested   (a <> a') b l r
    (Nested a b l r , n2                ) -> Nested a b l $ r <> n2

instance Semigroup a => Semigroup (Nests b a) where
  (<>) = curry $ \case
    (Unnested a     , Unnested a'       ) -> Unnested (a SG.<> a')
    (Unnested a     , Nested   a' b l r ) -> Nested   (a SG.<> a') b l r
    (Nested a b l r , n2                ) -> Nested a b l $ r SG.<> n2

_Leftmost :: Lens' (Nests b a) a
_Leftmost = singular $ _Unnested `failing` _Nested._1

_Rightmost :: Lens' (Nests b a) a
_Rightmost = singular $ _Unnested `failing` _Nested._4._Rightmost

_Nesters :: Traversal (Nests b a) (Nests c a) b c
_Nesters f = go
  where
  go = \case
    Unnested a     -> pure $ Unnested a
    Nested a b l r -> Nested a <$> f b <*> go l <*> go r

{-
nest :: (s -> s -> Maybe b) -> Splits s a -> [Nests b a]
nest f = go
  where
  go = \case
    Unsplit a    -> return $ Unnested a
    Split a s sp -> do
-}

{-

split :: (a -> Maybe (a,b,a)) -> a -> Splits b a
split f = go
  where
  go a = case f a of
    Just (a1,b,rest) -> Split a1 b $ go rest
    _                -> Unsplit a

resplit :: (a -> Maybe (a,b,a)) -> Splits b a -> Splits b a
resplit f = go
  where
  go = \case
    Unsplit a     -> case f a of
      Just (a1,b,a2) -> Split a1 b $ Unsplit a2
      _              -> Unsplit a
    Split a b (go -> s) -> case f a of
      Just (a1,b',a2) -> Split a1 b' $ Split a2 b s
      _               -> Split a b s

unsplit :: (a -> b -> a -> Maybe a) -> Splits b a -> Splits b a
unsplit f = go
  where
  go = \case
    Unsplit a    -> Unsplit a
    Split a1 b s -> case f a1 b $ s^._Leftmost of
      Just a -> s & _Leftmost .~ a
      _      -> Split a1 b $ go s
-}

