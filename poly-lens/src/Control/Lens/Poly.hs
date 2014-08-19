{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Lens.Poly where

import Control.Arrow ((***),(&&&))
import Control.Lens
import Data.Monoid
import GHC.Exts (Constraint)

class PolyEquality tg s t a b | tg s -> a, tg t -> b, tg s b -> t, tg t a -> s where
  equalityAt :: tg -> Equality s t a b

class PolyIso tg s t a b | tg s -> a, tg t -> b, tg s b -> t, tg t a -> s where
  isoAt :: tg -> Iso s t a b

class PolyLens tg s t a b | tg s -> a, tg t -> b, tg s b -> t, tg t a -> s where
  lensAt :: tg -> Lens s t a b

class PolyPrism tg s t a b | tg s -> a, tg t -> b, tg s b -> t, tg t a -> s where
  prismAt :: tg -> Prism s t a b

class PolyReview tg s t a b | tg s -> a, tg t -> b, tg s b -> t, tg t a -> s where
  reviewAt :: tg -> Review s t a b

class PolyTraversal tg s t a b | tg s -> a, tg t -> b, tg s b -> t, tg t a -> s where
  traversalAt :: tg -> Traversal s t a b

class PolySetter tg s t a b | tg s -> a, tg t -> b, tg s b -> t, tg t a -> s where
  setterAt :: tg -> Setter s t a b

class PolyGetter tg s a | tg s -> a, tg a -> s where
  getterAt :: tg -> Getter s a

class PolyFold tg s a | tg s -> a, tg a -> s where
  foldAt :: tg -> Fold s a

class PolyAction tg m s a | tg m s -> a, tg m a -> s where
  actionAt :: tg -> Action m s a

class PolyMonadicFold tg m s a | tg m s -> a, tg m a -> s where
  monadicFoldAt :: tg -> MonadicFold m s a

type Simple' (c :: * -> * -> * -> * -> Constraint) s a = c s s a a

newtype Foo a = Foo
  { _foo :: a
  }
makeLenses ''Foo

instance PolyIso (x -> Foo x) (Foo a) (Foo b) a b where
  isoAt _ = foo

data Bar a
  = Bar1 a
  | Bar2 Int
makePrisms ''Bar

instance PolyPrism (x -> Bar x) (Bar a) (Bar b) a b where
  prismAt _ = _Bar1

instance PolyPrism (Int -> Bar x) (Bar a) (Bar a) Int Int where
  prismAt _ = _Bar2

bar0 :: Bar Bool
bar0 = Bar1 True

bar1 :: Bar Bool
bar1 = Bar2 3

b0, b1 :: Maybe Bool
b0 = bar0 ^? prismAt Bar1
b1 = bar1 ^? prismAt Bar1

i0, i1 :: Maybe Int
i0 = bar0 ^? prismAt Bar2
i1 = bar1 ^? prismAt Bar2

(^%) :: s -> LensLike (Const a) s t a b -> a
s ^% l = s ^. coerced l

view' :: LensLike (Const a) s t a b -> s -> a
view' = flip (^%)

(^.%) :: b -> AnIso s t a b -> t
s ^.% l = s ^. coerced (from l)

viewFrom' :: AnIso s t a b -> b -> t
viewFrom' = flip (^.%)

(.>.) :: (PolyIso f s t a b, PolyIso g a b c d)
  => f -> g -> Iso s t c d
f .>. g = isoAt f . isoAt g

branched :: (PolyIso f s t a b, PolyIso g s t c d, PolyIso h s t (s,s) (t,t))
  => f -> g -> h -> Iso s t (a,c) (b,d)
branched f g h = isoAt h . (isoAt f `bimapping` isoAt g)

newtype Id a = Id
  { _runId :: a
  }
makeLenses ''Id

data Par = Par
data Dbl = Dbl

instance Monoid t => PolyIso Dbl s t (s,s) (t,t) where
  isoAt _ = iso (id &&& id) $ uncurry mappend

data Even = Even
instance PolyIso Even (Foo Bool) (Foo Bool) Int Int where
  isoAt _ = iso (\(Foo b) -> if b then 0 else 1) (Foo . even)

data Odd  = Odd
instance PolyIso Odd (Foo Bool) (Foo Bool) Int Int where
  isoAt _ = iso (\(Foo b) -> if b then 1 else 0) (Foo . odd)

instance PolyIso (x -> Sum x) a b (Sum a) (Sum b) where
  isoAt _ = iso Sum getSum

