{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Data.Sequence1 where

import Prelude hiding (map, length)
import qualified Prelude

import Control.Applicative
import Control.Arrow ((***),first,second)
import qualified Control.Lens as Lens
import Control.Lens hiding ((<.>), (<|), (|>), _head, _tail, _init, _last)
import Data.Function (on)
import Data.Functor.Apply (Apply (..))
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Semigroup (Semigroup)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as SG
import Data.Semigroup.Foldable (Foldable1)
import qualified Data.Semigroup.Foldable as SG

-- Sequence1 {{{

class Sequence1 f where
  type Seq1From t f a
  fromHT :: a -> Seq a -> f a
  fromIL :: Seq a -> a -> f a
  fromList :: Seq1From [] f a -> f a
  singleton :: a -> f a
  isSingleton :: f a -> Bool
  -- XXX : possible to combine >< and splitAt into lens?
  (><) :: f a -> f a -> f a
  splitAt :: Int -> f a -> (Maybe (f a), Maybe (f a))

  _Seq1Focus :: Lens' (f a) a
  _Seq1Rest :: Lens' (f a) (Seq a)
  _Seq1 :: Iso (f a) (f b) (Seq1From Seq f a) (Seq1From Seq f b)
  _FromSeq :: Prism (Seq a) (Seq b) (f a) (f b)
  _FromList :: Prism [a] [b] (f a) (f b)
  _FromNonEmpty :: Iso (f a) (f b) (NonEmpty a) (NonEmpty b)
  _Seq1Ix :: Int -> Lens' (f a) (Maybe a)
  _Seq1Cons :: Prism' (f a) (a,f a)
  _Seq1Snoc :: Prism' (f a) (f a,a)

  _head :: Lens' (f a) a
  _tail :: Lens' (f a) (Seq a)
  _init :: Lens' (f a) (Seq a)
  _last :: Lens' (f a) a

(<|) :: Sequence1 f => a -> f a -> f a
(<|) = curry (simply review _Seq1Cons)

(|>) :: Sequence1 f => f a -> a -> f a
(|>) = curry (simply review _Seq1Snoc)

length :: Foldable f => f a -> Int
length = lengthOf folded

-- }}}

-- Seq1L {{{

data Seq1L a = Seq1L
  { _seq1lFocus :: a
  , _seq1lRest :: Seq a
  }

makeLenses ''Seq1L

instance Sequence1 Seq1L where

  type Seq1From t Seq1L a = (a,t a)
  fromList (h,t) = Seq1L h $ S.fromList t
  fromHT = Seq1L
  fromIL = curry $ uncurry Seq1L . view (from htil)

  _Seq1Focus = seq1lFocus
  _Seq1Rest = seq1lRest

  _Seq1 = iso ((,) <$> view seq1lFocus <*> view seq1lRest) (uncurry Seq1L)

  _FromSeq = prism toSeq fromSeq
    where
    toSeq = (S.<|) <$> view seq1lFocus <*> view seq1lRest
    fromSeq = maybe (Left S.empty) (Right . uncurry Seq1L) . uncons

  _FromList = prism toL fromL
    where
    toL = (Lens.<|) <$> view seq1lFocus <*> (F.toList . view seq1lRest)
    fromL = maybe (Left []) (Right . uncurry Seq1L . second S.fromList) . uncons

  _FromNonEmpty = iso toNonEmpty fromNonEmpty
    where
    toNonEmpty = (:|) <$> view seq1lFocus <*> (F.toList . view seq1lRest)
    fromNonEmpty = Seq1L <$> NE.head <*> (S.fromList . NE.tail)

  singleton a = Seq1L a S.empty
  isSingleton = nullOf seq1lRest

  s1 >< s2 = s1 & seq1lRest %~ (S.>< review _FromSeq s2)

  splitAt n s@(Seq1L h t)
    | n == 0
    = (Nothing,Just s)
    | n <= S.length t
    = (Just . Seq1L h) *** (preview _FromSeq)
      $ S.splitAt (n - 1) t
    | otherwise
    = (Just s,Nothing)

  _Seq1Ix i f s@(Seq1L h t)
    | i == 0
    = maybe s (`Seq1L` t) <$> f (Just h)
    | i <= S.length t
    = (Seq1L h . flip (S.update it) t . fromMaybe a) <$> f (Just a)
    | otherwise = s <$ f Nothing
    where
    it = i - 1
    a = S.index t it

  _Seq1Cons = _Cons
  _Seq1Snoc = _Snoc

  _head = _Seq1 . _1
  _tail = _Seq1 . _2
  _init = _Seq1 . htil . _1
  _last = _Seq1 . htil . _2

htil :: Iso (a,Seq a) (b,Seq b) (Seq a,a) (Seq b,b)
htil = iso toIL toHT
  where
  toIL (h,t) = case S.viewr t of
    t' S.:> l -> (h S.<| t',l)
    _ -> (S.empty,h)
  toHT (i,l) = case S.viewl i of
    h S.:< i' -> (h,i' S.|> l)
    _ -> (l,S.empty)

instance Eq a => Eq (Seq1L a) where
  xs == ys = length xs == length ys
          && F.toList xs == F.toList ys

instance Ord a => Ord (Seq1L a) where
  compare = curry $ (<>) <$> uncurry fhead <*> uncurry ftail
    where
    fhead = compare `on` view seq1lFocus
    ftail = compare `on` view seq1lRest

instance Show a => Show (Seq1L a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (F.toList xs)

instance Semigroup (Seq1L a) where
  (<>) = (><)

instance Functor Seq1L where
  fmap f (Seq1L h t) = Seq1L (f h) (fmap f t)

instance FunctorWithIndex Int Seq1L where
  imap f (Seq1L h t) = Seq1L (f 0 h) $ imap f' t
    where
    f' i = f (i + 1)

instance Foldable Seq1L where
  foldr f z (Seq1L h t) = f h $ F.foldr f z t
  foldl f z (Seq1L h t) = F.foldl f (f z h) t

instance Foldable1 Seq1L where
  foldMap1 f (Seq1L h t)
    | t1 S.:< t' <- S.viewl t
    = f h SG.<> SG.foldMap1 f (Seq1L t1 t')
    | otherwise
    = f h

instance FoldableWithIndex Int Seq1L where
  ifoldMap f (Seq1L h t) = f 0 h <> ifoldMap f' t
    where
    f' i = f (i + 1)

instance Traversable Seq1L where
  traverse f (Seq1L h t) = Seq1L <$> f h <*> traverse f t

instance Traversable1 Seq1L where
  traverse1 f (Seq1L h t)
    | t1 S.:< t' <- S.viewl t
    = (<|) <$> f h <.> traverse1 f (Seq1L t1 t')
    | otherwise
    = singleton <$> f h

instance TraversableWithIndex Int Seq1L where
  itraverse f (Seq1L h t) = Seq1L <$> f 0 h <*> itraverse f' t
    where
    f' i = f (i + 1)

instance Applicative Seq1L where
  pure = singleton
  Seq1L fh ft <*> xs = F.foldl' add (fmap fh xs) ft
    where
    add ys f = ys >< fmap f xs

instance Monad Seq1L where
  return = singleton
  Seq1L h t >>= f = F.foldl' add (f h) t
    where
    add ys x = ys >< f x

instance Cons (Seq1L a) (Seq1L a) a a where
  _Cons = prism' (uncurry kahns) unkahns
    where
    kahns h (Seq1L t1 t) = Seq1L h $ t1 S.<| t
    unkahns (Seq1L h t) = case uncons t of
      Just (t1,t') -> Just (h,Seq1L t1 t')
      _ -> Nothing 

instance Snoc (Seq1L a) (Seq1L a) a a where
  _Snoc = prism' (uncurry snack) unsnack
    where
    snack (Seq1L h t) tn = Seq1L h $ t S.|> tn
    unsnack (Seq1L h t) = case unsnoc t of
      Just (t',tn) -> Just (Seq1L h t',tn)
      _ -> Nothing

type instance Index (Seq1L a)   = Int
type instance IxValue (Seq1L a) = a
instance Ixed (Seq1L a) where
  ix i f s@(Seq1L h t)
    | i == 0
    = f h <&> (`Seq1L` t)
    | 1 <= i && i <= length s
    = f (s ^. tailIx i) <&> (s &) . set (tailIx i)
    | otherwise
    = pure s
    where
    tailIx ind = seq1lRest . singular (ix $ ind - 1)
  {-# INLINE ix #-}

instance Each (Seq1L a) (Seq1L b) a b where

-- }}}

-- Seq1R {{{

data Seq1R a = Seq1R
  { _seq1rRest :: Seq a
  , _seq1rFocus :: a
  }

makeLenses ''Seq1R

instance Sequence1 Seq1R where

  type Seq1From t Seq1R a = (t a,a)
  fromList (i,l) = Seq1R (S.fromList i) l
  fromHT = curry $ uncurry Seq1R . view htil
  fromIL = Seq1R

  _Seq1Focus = seq1rFocus
  _Seq1Rest = seq1rRest

  _Seq1 = iso ((,) <$> view seq1rRest <*> view seq1rFocus) (uncurry Seq1R)

  _FromSeq = prism toSeq fromSeq
    where
    toSeq = (S.|>) <$> view seq1rRest <*> view seq1rFocus
    fromSeq = maybe (Left S.empty) (Right . uncurry Seq1R) . unsnoc

  _FromList = prism toL fromL
    where
    toL = (Lens.|>) <$> (F.toList . view seq1rRest) <*> view seq1rFocus
    fromL = maybe (Left []) (Right . uncurry Seq1R . first S.fromList) . unsnoc

  _FromNonEmpty = iso toNonEmpty fromNonEmpty
    where
    toNonEmpty = ((:|) <$> head <*> tail) . review _FromList
    fromNonEmpty = Seq1R <$> (S.fromList . NE.init) <*> NE.last

  singleton = Seq1R S.empty

  isSingleton = nullOf seq1rRest

  s1 >< s2 = s2 & seq1rRest %~ (review _FromSeq s1 S.><)

  splitAt n s@(Seq1R i l)
    | n == 0
    = (Nothing,Just s)
    | 0 < n && n <= S.length i
    = (preview _FromSeq) *** (Just . (`Seq1R` l))
      $ S.splitAt (n - 1) i
    | otherwise
    = (Just s,Nothing)

  _Seq1Ix x f s@(Seq1R i l)
    | 0 <= x && x < ln
    = ((`Seq1R` l) . flip (S.update x) i . fromMaybe a) <$> f (Just a)
    | x == ln
    = maybe s (Seq1R i) <$> f (Just l)
    | otherwise
    = s <$ f Nothing
    where
    ln = S.length i
    a = S.index i x

  _Seq1Cons = _Cons
  _Seq1Snoc = _Snoc

  _head = _Seq1 . from htil . _1
  _tail = _Seq1 . from htil . _2
  _init = _Seq1 . _1
  _last = _Seq1 . _2

instance Eq a => Eq (Seq1R a) where
  xs == ys = length xs == length ys
          && F.toList xs == F.toList ys

instance Ord a => Ord (Seq1R a) where
  compare = curry $ (<>) <$> uncurry finit <*> uncurry flast
    where
    finit = compare `on` view seq1rRest
    flast = compare `on` view seq1rFocus

instance Show a => Show (Seq1R a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (F.toList xs)

instance Semigroup (Seq1R a) where
  (<>) = (><)

instance Functor Seq1R where
  fmap f (Seq1R i l) = Seq1R (fmap f i) (f l)

instance FunctorWithIndex Int Seq1R where
  imap f (Seq1R i l) = Seq1R (imap f i) $ f' l
    where
    f' = f $ length i

instance Foldable Seq1R where
  foldr f z (Seq1R i l) = F.foldr f (f l z) i
  foldl f z (Seq1R i l) = f (F.foldl f z i) l

instance Foldable1 Seq1R where
  foldMap1 f (Seq1R i l)
    | i1 S.:< i' <- S.viewl i
    = f i1 SG.<> SG.foldMap1 f (Seq1R i' l)
    | otherwise
    = f l

instance FoldableWithIndex Int Seq1R where
  ifoldMap f (Seq1R i l) = ifoldMap f i <> f (length i) l

instance Traversable Seq1R where
  traverse f (Seq1R i l) = Seq1R <$> traverse f i <*> f l

instance Traversable1 Seq1R where
  traverse1 f (Seq1R i l)
    | i1 S.:< i' <- S.viewl i
    = (<|) <$> f i1 <.> traverse1 f (Seq1R i' l)
    | otherwise
    = singleton <$> f l

instance TraversableWithIndex Int Seq1R where
  itraverse f (Seq1R i l) = Seq1R <$> itraverse f i <*> f (length i) l

instance Applicative Seq1R where
  pure = singleton
  Seq1R fi fl <*> xs = case S.viewl fi of
    fi1 S.:< fi' -> add (F.foldl' add (fmap fi1 xs) fi') fl
    _ -> fmap fl xs
    where
    add ys f = ys >< fmap f xs

instance Monad Seq1R where
  return = singleton
  Seq1R i l >>= f = case S.viewl i of
    i1 S.:< i' -> add (F.foldl' add (f i1) i') l
    _ -> f l
    where
    add ys x = ys >< f x

instance Cons (Seq1R a) (Seq1R a) a a where
  _Cons = prism' (uncurry kahns) unkahns
    where
    kahns i1 (Seq1R i l) = Seq1R (i1 S.<| i) l
    unkahns (Seq1R i l) = second (`Seq1R` l) <$> uncons i

instance Snoc (Seq1R a) (Seq1R a) a a where
  _Snoc = prism' (uncurry snack) unsnack
    where
    snack (Seq1R i l) l' = Seq1R (i S.|> l) l'
    unsnack (Seq1R i l) = case unsnoc i of
      Just (i',ifinal) -> Just (Seq1R i' ifinal,l)
      _ -> Nothing

type instance Index (Seq1R a)   = Int
type instance IxValue (Seq1R a) = a
instance Ixed (Seq1R a) where
  ix n f s@(Seq1R i l)
    | 0 <= n && n < ln
    = f (s ^. tailIx n) <&> (s &) . set (tailIx n)
    | n == ln
    = f l <&> (Seq1R i)
    | otherwise
    = pure s
    where
    ln = length s
    tailIx ind = seq1rRest . singular (ix ind)
  {-# INLINE ix #-}

instance Each (Seq1R a) (Seq1R b) a b where

-- }}}

viewl :: Sequence1 s => s a -> (a,Maybe (s a))
viewl s = (s^._head,s^._tail.pre _FromSeq)

viewr :: Sequence1 s => s a -> (Maybe (s a),a)
viewr s = (s^._init.pre _FromSeq,s^._last)

