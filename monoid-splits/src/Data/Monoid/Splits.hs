{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Data.Monoid.Splits where

import Control.Lens.Indexed.Hist

import Control.Applicative
import Control.Lens hiding ((<.>))
import Data.Ord (comparing)
import Data.List (intersperse)
import Data.Monoid
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as S
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Functor.Bind
import Data.Semigroup.Foldable
import GHC.Exts (Constraint)

data Splits b a
  = Unsplit a
  | Split a b (Splits b a)
  deriving (Eq,Show)

makePrisms ''Splits

-- FFT {{{

instance Functor (Splits b) where
  fmap = second

instance Foldable (Splits b) where
  foldMap = bifoldMap (const mempty)

instance Traversable (Splits b) where
  traverse = bitraverse pure

-- }}}

-- Bi FFT {{{

instance Bifunctor Splits where
  bimap f g = go
    where
    go = \case
      Unsplit a   -> Unsplit $ g a
      Split a b s -> Split (g a) (f b) $ go s

instance Bifoldable Splits where
  bifoldMap f g = go
    where
    go = \case
      Unsplit a   -> g a
      Split a b s -> g a <> f b <> go s

instance Bitraversable Splits where
  bitraverse f g = go
    where
    go = \case
      Unsplit a   -> Unsplit <$> g a
      Split a b s -> Split   <$> g a <*> f b <*> go s

-- }}}

{-
-- Stack {{{

newtype Stack a = Stack
  { getStack :: [a]
  } deriving (Show)

makeLensesWith
  (isoRules
  & lensIso .~ Just . ('_' :)
  ) ''Stack

stackEq :: Eq a => Stack a -> Stack a -> Bool
stackEq (Stack as) (Stack bs) = as == bs

instance Eq (Stack a) where
  Stack as == Stack bs = length as == length bs

instance Ord (Stack a) where
  Stack as `compare` Stack bs = comparing length as bs

instance Monoid (Stack a) where
  mempty = Stack []
  Stack as `mappend` Stack bs = Stack $ as ++ bs

instance Cons (Stack a) (Stack b) a b where
  _Cons = from _Stack . _Cons . bimapping id _Stack

instance Snoc (Stack a) (Stack b) a b where
  _Snoc = from _Stack . _Snoc . bimapping _Stack id

-- }}}
-}

-- Ixed FFT {{{

type SpIx b                        = Maybe b
type EqSpIx  b                     = Eq b
type OrdSpIx b                     = Ord b

initSpIx :: SpIx b
initSpIx = Nothing

spIxEq :: EqSpIx b => SpIx b -> SpIx b -> Bool
spIxEq = (==)

(<+) :: b -> SpIx b -> SpIx b
(<+) = (<$)
infixr 5 <+



type instance Index   (Splits b a) = SpIx b
type instance IxValue (Splits b a) = a

instance EqSpIx b => Ixed (Splits b a) where
  ix bs f = itraverse $ \bs' -> if bs `spIxEq` bs' then f else pure

instance FunctorWithIndex (SpIx b) (Splits b) where
  imap f = go initSpIx
    where
    go i = \case
      Unsplit a   -> Unsplit $ f i a
      Split a b s -> Split (f i a) b $ go (b <+ i) s
  
instance FoldableWithIndex (SpIx b) (Splits b) where
  ifoldMap f = go initSpIx
    where
    go i = \case
      Unsplit a   -> f i a
      Split a b s -> f i a <> go (b <+ i) s

instance TraversableWithIndex (SpIx b) (Splits b) where
  itraverse f = go initSpIx
    where
    go i = \case
      Unsplit a   -> Unsplit <$> f i a
      Split a b s -> Split   <$> f i a <*> pure b <*> go (b <+ i) s

-- }}}

-- FT1 {{{

instance Foldable1 (Splits b) where
  foldMap1 f = go
    where
    go = \case
      Unsplit a   -> f a
      Split a _ s -> f a S.<> go s

instance Traversable1 (Splits b) where
  traverse1 f = go
    where
    go = \case
      Unsplit a   -> Unsplit      <$> f a
      Split a b s -> flip Split b <$> f a <.> go s

-- }}}

-- Traverse Min Max {{{

instance OrdSpIx b => TraverseMin (SpIx b) (Splits b) where
  traverseMin pafb = \case
    Unsplit a   -> Unsplit <$> f i0 a
    Split a b s -> Split   <$> f i0 a <*> pure b <*> pure s
    where
    i0 = initSpIx :: SpIx b
    f  = indexed pafb

instance OrdSpIx b => TraverseMax (SpIx b) (Splits b) where
  traverseMax pafb = go i0
    where
    i0 = initSpIx :: SpIx b
    go i = \case
      Unsplit a   -> Unsplit   <$> f i a
      Split a b s -> Split a b <$> go (b <+ i) s
    f = indexed pafb

-- }}}

-- Plated {{{

instance Plated (Splits b a) where
  plate f = \case
    Unsplit a   -> pure $ Unsplit a
    Split a b s -> Split a b <$> f s

-- }}}

-- Monoid Semigroup {{{

instance Monoid a => Monoid (Splits b a) where
  mempty  = Unsplit mempty
  mappend = curry $ \case
    (Unsplit a   , Unsplit a'      ) -> Unsplit (a <> a')
    (Unsplit a   , Split   a' b  s ) -> Split   (a <> a') b  s
    (Split a b s , s2              ) -> Split    a        b (s <> s2)

instance Semigroup a => Semigroup (Splits b a) where
  (<>) = curry $ \case
    (Unsplit a   , Unsplit a'      ) -> Unsplit (a S.<> a')
    (Unsplit a   , Split   a' b  s ) -> Split   (a S.<> a') b  s
    (Split a b s , s2              ) -> Split    a        b (s S.<> s2)

-- }}}

-- Cons Snoc {{{

instance Cons (Splits b a) (Splits d a) (a,b) (a,d) where
  _Cons = prism khans unkhans
    where
    khans ((a,b),s) = Split a b s
    unkhans = \case
      Unsplit a   -> Left $ Unsplit a
      Split a b s -> Right ((a,b),s)

instance Snoc (Splits b a) (Splits d a) (b,a) (d,a) where
  _Snoc = prism snack unsnack
    where
    snack (s,(b,a)) = go s
      where
      go = \case
        Unsplit a'     -> Split a' b $ Unsplit a
        Split a' b' s' -> Split a' b' $ go s'
    unsnack = \case
      Unsplit a      -> Left $ Unsplit a
      Split a_ b_ s_ -> Right $ go a_ b_ s_
        where
        go a b = \case
          Unsplit a'     -> (Unsplit a,(b,a'))
          Split a' b' s' -> first (Split a b) $ go a' b' s'

-- }}}

-- Reversing {{{

instance Reversing (Splits b a) where
  reversing = \case
    Unsplit a   -> Unsplit a
    Split a b s -> reversing s |> (b,a)

-- }}}

-- Traversals {{{

_Leftmost :: OrdSpIx b => IndexedLens' (SpIx b) (Splits b a) a
_Leftmost = singular traverseMin

_Rightmost :: OrdSpIx b => IndexedLens' (SpIx b) (Splits b a) a
_Rightmost = singular traverseMax

_Splits :: IndexedTraversal (SpIx b) (Splits b a) (Splits b c) a c
_Splits = itraversed

_Splitters :: IndexedTraversal (SpIx b) (Splits b a) (Splits c a) b c
_Splitters (pbfc :: p b (f c)) = go initSpIx
  where
  f = indexed pbfc
  go i = \case
    Unsplit a   -> pure $ Unsplit a
    Split a b s -> Split a <$> f i b <*> go (b <+ i) s

-- }}}

-- Construction Deconstruction {{{

split :: (a -> Maybe (a,b,a)) -> a -> Splits b a
split f = go
  where
  go a = case f a of
    Just (a1,b,rest) -> Split a1 b $ go rest
    _                -> Unsplit a

unsplit :: OrdSpIx b => (a -> b -> a -> a) -> Splits b a -> a
unsplit f = go
  where
  go = \case
    Unsplit a   -> a
    Split a b s -> go $ s & _Leftmost .~ a'
      where
      a' = f a b $ s^._Leftmost

expand :: (a -> Maybe (a,b,a)) -> Splits b a -> Splits b a
expand f = go
  where
  go = \case
    Unsplit a     -> case f a of
      Just (a1,b,a2) -> Split a1 b $ Unsplit a2
      _              -> Unsplit a
    Split a b (go -> s) -> case f a of
      Just (a1,b',a2) -> Split a1 b' $ Split a2 b s
      _               -> Split a b s

compress :: OrdSpIx b => (a -> b -> a -> Maybe a) -> Splits b a -> Splits b a
compress f = go
  where
  go = \case
    Unsplit a    -> Unsplit a
    Split a b s -> case f a b $ s^._Leftmost of
      Just a' -> go $ s & _Leftmost .~ a'
      _       -> Split a b $ go s

-- }}}

accumIndex :: FunctorWithIndex i f => f a -> f (i,a)
accumIndex = imap (,)

accumIndices :: (FunctorWithIndex i f, Reversing (f a), Reversing (f (a,i))) => f a -> f (i,(a,i))
accumIndices = imap (,) . reversing . imap (flip (,)) . reversing

unwrapStacks :: Functor f => f (Stack b,(a,Stack b)) -> f ([b],a,[b])
unwrapStacks = fmap $ \(Stack as,(a,Stack bs)) -> (as,a,bs)

{-
splitStacks :: (Index (Splits b a) ~ Stack b)
  => Splits b a -> Splits b ([b],a,[b])
splitStacks = unwrapStacks . accumIndices
-}

onElem :: Eq a => [a] -> [a] -> Maybe ([a],a,[a])
onElem as ls = case break (`elem` as) ls of
  (as1,a:as2) -> Just (as1,a,as2)
  _           -> Nothing

onParens :: String -> Maybe (String,Char,String)
onParens = onElem "()"

prettySplits :: Splits String String -> String
prettySplits = prettySplitsWith' id

prettySplitsWith' :: (b -> String) -> Splits b String -> String
prettySplitsWith' f = prettySplitsWith f id

prettySplitsWith :: (b -> String) -> (a -> String) -> Splits b a -> String
prettySplitsWith = bifoldMap

renderStack :: ([Char],String,[Char]) -> String
renderStack (as,a,bs) = "\ESC[4m«"++go as++"»"++a++"«"++go bs++"»\ESC[m"
  where
  go = intersperse ','

{-
prettySplitsHist :: (Index (Splits Char String) ~ Stack Char)
  => Splits Char String -> IO ()
prettySplitsHist = putStrLn . prettySplitsWith (:[]) renderStack . splitStacks
-}

