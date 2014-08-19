{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}

module Control.Lens.Prism.Total where

import Prelude hiding (id,(.))
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Lens hiding ((<.>))
import Control.Lens.Internal.Bazaar
import Control.Lens.Internal.Context
import Control.Lens.Internal.Prism
import Control.Monad.State
import Data.Profunctor.Rep
import Data.Profunctor.Unsafe
import Data.Functor.Apply
import Data.Functor.Compose
import GHC.TypeLits
import Data.Tagged
import Data.Proxy


-- Member {{{

data MemEvid a as where
  Head :: MemEvid a (a ': as)
  Tail :: MemEvid a as -> MemEvid a (b ': as)

instance Show (MemEvid a as) where
  showsPrec d = \case
    Head   -> showString "Head"
    Tail e -> showParen (d > 10)
      $ showString "Tail "
      . showsPrec 11 e

class Member a as where
  memEvid :: MemEvid a as

instance Member a (a ': as) where
  memEvid = Head

instance Member a as => Member a (b ': as) where
  memEvid = Tail memEvid

-- }}}

class HasTag (nm :: Symbol) (f :: * -> *) where

instance HasTag nm (Const a)
instance Member nm nms => HasTag nm (Tagged nms)

class HasTags (nm :: [Symbol]) (f :: * -> *) where

instance HasTags '[] f where

instance (HasTag nm f, HasTags nms f) => HasTags (nm ': nms) f where

instance HasTags nms (Tagged nms)
instance (HasTags nms f, HasTags nms g) => HasTags nms (Compose f g)

type family Union (as :: [k]) (bs :: [k]) :: [k] where
  Union '[] bs       = bs
  Union (a ': as) bs = a ': Union as (Remove a bs)

type family Remove (a :: k) (as :: [k]) :: [k] where
  Remove a '[]       = '[]
  Remove a (a ': as) = Remove a as
  Remove a (b ': as) = b ': Remove a as

-- TaggedPrism {{{

type TaggedPrism nm s t a b = forall (p :: * -> * -> *) (f :: * -> *).
  (Choice p, Applicative f, HasTag nm f)
  => p a (f b) -> p s (f t)

type ATaggedPrism nm s t a b = Market a b a (Tagged nm b) -> Market a b s (Tagged nm t)

tprism :: Proxy nm -> (b -> t) -> (s -> Either t a) -> TaggedPrism nm s t a b
tprism !Proxy bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE tprism #-}

tprism' :: Proxy nm -> (b -> s) -> (s -> Maybe a) -> TaggedPrism nm s s a b
tprism' !Proxy bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE tprism' #-}

-- }}}



class FieldName nm s t a b | s nm -> a, t nm -> b, s b nm -> t, t a nm -> s where
  _field :: Proxy nm -> TaggedPrism nm s t a b

-- Traversal Internals {{{

newtype Tag ts f a b c d = Tag
  { unTag :: f a b c d
  }

newtype TagT ts f a b c d e = TagT
  { unTagT :: f a b c d e
  }

type TaggedTraversing (nms :: [Symbol]) p f s t a b =
  Over p (TagT nms BazaarT p f a b) s t a b

instance Functor (f a b c d) => Functor (TagT ts f a b c d) where
  fmap f (TagT m) = TagT $ fmap f m
  {-# INLINE fmap #-}

instance IndexedFunctor (f a b) => IndexedFunctor (TagT ts f a b) where
  ifmap f (TagT m) = TagT $ ifmap f m
  {-# INLINE ifmap #-}

instance IndexedComonad (f a b) => IndexedComonad (TagT ts f a b) where
  iextract (TagT m) = iextract m
  {-# INLINE iextract #-}
  iduplicate (TagT m) = TagT $ ifmap TagT $ iduplicate m
  {-# INLINE iduplicate #-}

instance Sellable a (f a b) => Sellable a (TagT ts f a b) where
  sell = rmap TagT sell
  {-# INLINE sell #-}

instance Bizarre a (f a b) => Bizarre a (TagT ts f a b) where
  bazaar g (TagT m) = bazaar g m
  {-# INLINE bazaar #-}

instance Apply (f a b c d) => Apply (TagT ts f a b c d) where
  TagT mf <.> TagT ma = TagT $ mf <.> ma

instance Applicative (f a b c d) => Applicative (TagT ts f a b c d) where
  pure = TagT . pure
  {-# INLINE pure #-}
  TagT mf <*> TagT ma = TagT $ mf <*> ma
  {-# INLINE (<*>) #-}


pins :: (Bizarre p w, Corepresentable p) => w a b t -> [Corep p a]
pins = getConst #. bazaar (cotabulate $ \ra -> Const [ra])
{-# INLINE pins #-}

unsafeOuts :: (Bizarre p w, Corepresentable p) => w a b t -> [b] -> t
unsafeOuts = evalState `rmap` bazaar (cotabulate (\_ -> state (unconsWithDefault fakeVal)))
  where fakeVal = error "unsafePartsOf': not enough elements were supplied"
{-# INLINE unsafeOuts #-}

unconsWithDefault :: a -> [a] -> (a,[a])
unconsWithDefault d []     = (d,[])
unconsWithDefault _ (x:xs) = (x,xs)
{-# INLINE unconsWithDefault #-}

failing_ :: (Conjoined p, Applicative f, ns3 ~ Union ns1 ns2, HasTags ns3 f)
  => TaggedTraversing ns1 p f s t a b
  -> TaggedTraversing ns2 p f s t a b
  -> Over p f s t a b
failing_ l r pafb s = case pins b of
  [] -> runBazaarT (unTagT $ r sell s) pafb
  xs -> unsafeOuts b <$> traverse (corep pafb) xs
  where b = l sell s
infixl 5 `failing_`

-- }}}

-- Test {{{

data Foo
  = Foo Int
  | Bar Int
  deriving (Eq,Show)

foo :: Proxy "Foo"
foo = Proxy
bar :: Proxy "Bar"
bar = Proxy

instance FieldName "Foo" Foo Foo Int Int where
  _field !p = tprism' p Foo $ \case
    Foo i -> Just i
    _     -> Nothing

instance FieldName "Bar" Foo Foo Int Int where
  _field !p = tprism' p Bar $ \case
    Bar i -> Just i
    _     -> Nothing

ex1 :: Foo
ex1 = Foo 3

ex2 :: Foo
ex2 = Bar 4

-- }}}

