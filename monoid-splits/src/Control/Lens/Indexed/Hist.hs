{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Control.Lens.Indexed.Hist where

import Control.Applicative
import Control.Comonad
import Control.Lens hiding ((<.>))
import Control.Lens.Internal.Context
import Control.Monad.Reader (MonadReader)
import Data.Functor.Bind
import Data.Profunctor
import Data.Profunctor.Rep

import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T

import Data.Monoid
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as S
import Data.Ord (comparing)

-- Stack {{{

newtype Stack a = Stack
  { getStack :: [a]
  } deriving (Show)

_Stack :: Iso1' [] Stack
_Stack = iso Stack getStack

-- Equality of Stackories is limited to a length comparison...
instance Eq (Stack a) where
  Stack as == Stack bs = length as == length bs

-- ... as is Ordering comparison.
instance Ord (Stack a) where
  Stack as `compare` Stack bs = comparing length as bs



instance Functor Stack where
  fmap = over $ _Stack \. mapped

instance Foldable Stack where
  foldMap = foldMapOf $ _Stack \. folded

instance Traversable Stack where
  traverse = traverseOf $ _Stack \. traversed



instance Semigroup (Stack a) where
  (<>) = liftI2 _Stack (S.<>)

instance Monoid (Stack a) where
  mempty  = Stack mempty
  mappend = liftI2 _Stack (<>)



instance Cons (Stack a) (Stack b) a b where
  _Cons = _Stack \. _Cons . _Second _Stack

instance Snoc (Stack a) (Stack b) a b where
  _Snoc = _Stack \. _Snoc . _First _Stack

instance Reversing (Stack a) where
  reversing = under _Stack reversing



instance Plated (Stack a) where
  plate = from _Stack ... _Stack

-- }}}

-- Hist {{{

newtype Hist f a = Hist
  { unHist :: f a
  } deriving (Eq,Show)

_Hist :: Iso1 f g (Hist f) (Hist g)
_Hist = iso Hist unHist

instance Functor f => Functor (Hist f) where
  fmap     = over $ _Hist \. mapped

instance Foldable f => Foldable (Hist f) where
  foldMap  = foldMapOf $ _Hist \. folded

instance Traversable f => Traversable (Hist f) where
  traverse = traverseOf $ _Hist \. traversed



instance Applicative f => Applicative (Hist f) where
  pure  = Hist . pure
  (<*>) = liftI2 _Hist (<*>)

instance Monad f => Monad (Hist f) where
  return  = Hist . return
  m >>= f = Hist $ unHist m >>= unHist . f

instance Apply f => Apply (Hist f) where
  (<.>) = liftI2 _Hist (<.>)

instance Comonad f => Comonad (Hist f) where
  extract   = extract   . unHist
  duplicate = fmap Hist . under _Hist duplicate



type instance Index   (Hist f a) = Stack (Index (f a))
type instance IxValue (Hist f a) = IxValue      (f a)

instance FunctorWithIndex i f => FunctorWithIndex (Stack i) (Hist f) where
  imap f (Hist m) = undefined

ixState :: forall p f i j s t a b.
  ( Indexable (j,i) p, Functor f)
  => j -> (j -> i -> (j,i))
  -> AnIndexedLens i s t a b
  -> p a (f b) -> s -> f t
ixState z f l pafb s = (undefined :: f t)
  where
  g :: (j,i) -> a -> f b
  g = indexed pafb
  l' :: Indexed i a (Pretext (Indexed i) a b b) -> s -> ((j,i) -> a -> f b) -> f t
  l' iab s' = runPretext (l iab s') . Indexed . lmap snd

-- }}}

-- Lens Utils {{{

type Iso1  f g h i = forall a b. Iso (f a) (g b) (h a) (i b)
type Iso1' f g     = Iso1 f f g g

type Iso2  f g h i = forall a b c d. Iso (f a b) (g c d) (h a b) (i c d)
type Iso2' f g     = Iso2 f f g g

liftI2 :: Iso1 f g h i -> (g a -> g b -> f c) -> i a -> i b -> h c
liftI2 l op a b = view_ l $ op (a ^\ l) (b ^\ l)

view_ :: MonadReader s m => Iso s t a b -> m a
view_ l = view $ coerced l

(^\) :: b -> Iso s t a b -> t
s ^\ l = s ^. coerced (from l)
infixl 8 ^\

(^/) :: s -> Iso s t a b -> a
s ^/ l = s ^. coerced l
infixl 8 ^/

(\.) :: (Profunctor p, Functor f) => Iso s t a b -> Optic p f t s c d -> Optic p f b a c d
l \. f = from l . f
infixr 9 \.

_First :: (Bifunctor q, Bifunctor r)
  => AnIso s t a b
  -> Iso (q s s') (r t t') (q a s') (r b t')
_First l = bimapping l id

_Second :: (Bifunctor q, Bifunctor r)
  => AnIso s t a b
  -> Iso (q s' s) (r t' t) (q s' a) (r t' b)
_Second l = bimapping id l

-- }}}

