{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

module Codata.Stream where

import Prelude hiding (id,(.))
import Control.Applicative
import Control.Category
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Class

import Data.Monoid

data Stream f a
  = a :> Stream f (f a)

instance ShowLen Bool where
  showsLPrec d b = case b of
    True -> str "True"
    _    -> str "False"

instance ShowLen Integer where
  showsLPrec _ = str . show

class ShowLen1 (f :: * -> *) where
  showsLPrec1 :: ShowLen a => Int -> f a -> ShowL

instance (ShowLen a, ShowLen1 f) => ShowLen (f a) where
  showsLPrec = showsLPrec1

instance ShowLen1 f => ShowLen1 (Stream f) where
  showsLPrec1 d (a :> s) = par (d > 10)
    $ showsLPrec 11 a <:> str " :> " <:> showsLPrec1 11 s

-- ShowLen {{{

class ShowLen a where
  showsLPrec :: Int -> a -> ShowL

type ShowL = Lim Char

str :: String -> ShowL
str = Lim . take'

par :: Bool -> ShowL -> ShowL
par b m = if b
  then str "(" <:> m <:> str ")"
  else m

showL :: ShowLen a => Integer -> a -> String
showL l a = snd $ lim (showsL a) l

showsL :: ShowLen a => a -> ShowL
showsL = showsLPrec 0

(<:>) :: Monoid m => m -> m -> m
(<:>) = (<>)
infixl 6 <:>

-- }}}

-- Lim {{{

data Lim a = Lim
 { lim :: Integer -> (Integer,[a])
 }

instance Monoid (Lim a) where
  mempty = Lim $ flip (,) []
  mappend l1 l2 = Lim $ \i0 ->
    let (i1,s1) = lim l1 i0
        (i2,s2) = lim l2 i1
    in (i2,s1 <> s2)

take' :: [a] -> Integer -> (Integer,[a])
take' as i
  | i <= 0 = (0,[])
  | otherwise = (i - toEnum (length as),take (fromEnum i) as)

-- }}}

instance (Monad m, Monoid a) => Monoid (ReaderT r m a) where
  mempty  = return mempty
  mappend = liftM2 mappend

instance (Monad m, Monoid a) => Monoid (StateT s m a) where
  mempty  = return mempty
  mappend = liftM2 mappend

instance (Monad m, Monoid a, Monoid w) => Monoid (WriterT w m a) where
  mempty  = return mempty
  mappend = liftM2 mappend

data Foo = Foo
  { foo :: [Int]
  , bar :: [Bool]
  }

baz :: Monoid m => (a -> m) -> a -> a -> m
baz f x y = f x <> f y

{-
mlift :: (ArrowProduct a, Monoid m) => a b m -> a (b,b) m
mlift f = f *** f >>> uncurry' (arr2 mappend)

instance Monoid Foo where
  mempty  = Foo mempty mempty
  mappend = undefined
    where
    -- x = liftM2 Foo

class Arrow a => ArrowProduct a where
  uncurry' :: a b (a c d) -> a (b,c) d
  curry'   :: a (b,c) d -> a b (a c d)

instance ArrowProduct (->) where
  uncurry' = uncurry
  curry'   = curry

instance Monad m => ArrowProduct (Kleisli m) where
  uncurry'  (Kleisli f) = Kleisli $ \(a,b) -> f a >>= flip runKleisli b
  curry'    (Kleisli f) = Kleisli $ \a -> return $ Kleisli $ \b -> f (a,b)

arr2 :: Arrow a => (b -> c -> d) -> a b (a c d)
arr2 f = genArr . arr f

genArr :: (Arrow a1, Arrow a2) => a1 (b -> c) (a2 b c)
genArr = arr arr

assocA :: Arrow a => a ((b,c),d) e -> a (b,(c,d)) e
assocA = (>>>) $ arr $ \(b,(c,d)) -> ((b,c),d)

unassocA :: Arrow a => a (b,(c,d)) e -> a ((b,c),d) e
unassocA = (>>>) $ arr $ \((b,c),d) -> (b,(c,d))

swapA :: Arrow a => a ((b,c),d) e -> a ((b,d),c) e
swapA = (>>>) $ arr $ \((b,c),d) -> ((b,d),c)

class Functor f => FunctorA (f :: * -> *) where
  fmapA :: Arrow ar => ar a b -> ar (f a) (f b)
-}

{-
instance FunctorA Maybe where
  fmapA a = _
-}

data Path
  = Root
  | L Path
  | R Path
  deriving (Eq,Show)

data I (p :: Path) where
  Root_ :: I Root
  L_    :: I z -> I (L z)
  R_    :: I z -> I (R z)

type family UnLeft (p :: Path) :: Path where
  UnLeft (L p) = p

type family UnRight (p :: Path) :: Path where
  UnRight (R p) = p

type family Up (p :: Path) :: Path where
  Up (L p) = p
  Up (R p) = p

data Z (p :: Path) a where
  Z :: a
    -> Z (L p) a
    -> Z (R p) a
    -> I p
    -> ((p ~ L p') => Z p' a)
    -> ((p ~ R p') => Z p' a)
    -> Z p a

node :: Z p a -> a
node (Z a _ _ _ _ _) = a

left :: Z p a -> Z (L p) a
left (Z _ l _ _ _ _) = l

right :: Z p a -> Z (R p) a
right (Z _ _ r _ _ _) = r

path :: Z p a -> I p
path (Z _ _ _ p _ _) = p

{-
up :: Z (f z) a -> Z z a
up Z{..} = case path of
  L_ _ -> unLeft
  R_ _ -> unRight
  _    -> error "actually total"
-}

{-
t0 :: Z Root Int
t0 = Z
  { node = 1
  , left = reLeft 
  }
-}

{-
reLeft :: Z p a -> Z (L p) a
reLeft t = Z
  { node  =           node   t
  , left  = reLeft  $ reLeft t
  , right = reRight $ reLeft t
  , path  =      L_ $ path   t
  , up    =
  }

reRight :: Z p a -> Z (R p) a
reRight t = Z
  { node  =           node    t
  , left  = reLeft  $ reRight t
  , right = reRight $ reRight t
  , path  =      R_ $ path    t
  , up    =           undefined
  }

instance Functor (Z p) where
  fmap f t = Z
    { node  = f  $  node    t
    , left  = f <$> left    t
    , right = f <$> right   t
    , path  =       path    t
    }

instance KnownPath p => Applicative (Z p) where
  pure a = t
    where
    t = Z
      { node  = a
      , left  = pure a
      , right = pure a
      , path  = knownPath
      }
  f <*> x = t
    where
    t = Z
      { node  = node  f  $  node  x
      , left  = left  f <*> left  x
      , right = right f <*> right x
      , path  = path  f
      }

class KnownPath (p :: Path) where
  knownPath :: I p
  upPath    :: KnownPath (Up p) => I (Up p)

instance KnownPath Root where
  knownPath = Root_
  upPath    = undefined

instance KnownPath p => KnownPath (L p) where
  knownPath = L_ knownPath
  upPath    = knownPath

instance KnownPath p => KnownPath (R p) where
  knownPath = R_ knownPath
  upPath    = knownPath

reRoot :: Z p a -> Z Root a
reRoot t = Z
  { node    = node  t
  , left    = left  $ reRoot t
  , right   = right $ reRoot t
  , path    = Root_
  }
-}

