{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module Data.Text.Printf where

import Prelude hiding (id,(.))
import Control.Arrow (Kleisli(..),Arrow(..))
import Control.Category
import Control.Monad
import Data.Proxy
import Data.Profunctor
import Data.Functor.Contravariant
import Data.Semigroupoid
import Data.Semigroupoid.Dual
import Data.Monoid (Monoid(..),(<>))
import Data.Char
import Data.List
import Data.Tuple

class Category r => Format r where
  lit  :: String -> r a a
  char :: r a (Char -> a)
  int  :: r a (Int  -> a)
  pp   :: PP b -> r a (b -> a)

class Format r => PreFormat r where
  bindZ :: r a (x -> b) -> (x -> r b c) -> r a c

class Format r => PostFormat r where
  post :: r a (b -> c) -> (b -> d) -> r a (d -> c)

-- PP {{{

data PP a = PP
  { ppPrint :: a -> String
  , ppParse :: String -> Maybe (a,String)
  }

_ShowRead :: (Show a, Read a) => PP a
_ShowRead = PP show $ single . reads

-- }}}

-- Convenience {{{

type Pre  (ts :: [*]) (x :: *) = forall r. PreFormat r  => r x (Inputs x ts)
type Post (ts :: [*]) (x :: *) = forall r. PostFormat r => r x (Inputs x ts)
type Fmt  (ts :: [*]) (x :: *) = forall r. Format r     => r x (Inputs x ts)

type family Inputs (x :: *) (ts :: [*]) :: * where
  Inputs x '[]       = x
  Inputs x (t ': ts) = t -> Inputs x ts

-- }}}

-- Printf {{{

newtype Pr r a b = Pr
  { runPr :: (r -> a) -> b
  }

printf :: Pr r r b -> b
printf (Pr f) = f id

instance Monoid m => Category (Pr m) where
  id    = Pr $ \k -> k mempty
  g . f = Pr
    $ \k -> runPr g
    $ \a -> runPr f
    $ \b -> k $ a <> b

instance Format (Pr String) where
  lit s = Pr ($ s)
  char  = Pr (. (:[]))
  int   = Pr (. show)
  pp p  = Pr (. ppPrint p)

{-
instance PreFormat (Pr String) where
  bindZ p f = Pr $ \k -> _
    where
    x = runPr p
-}

-- }}}

-- Scanf {{{

newtype Sc m r a b = Sc
  { unSc :: Dual (F ((,) r) (Kleisli m)) a b
  }

runSc :: Sc m r a b -> r -> b -> m (r,a)
runSc = curry . runKleisli . unF . getDual . unSc

liftSc :: (r -> b -> m (r,a)) -> Sc m r a b
liftSc = Sc . Dual . F . Kleisli . uncurry

instance Monad m => Category (Sc m r) where
  id    = Sc id
  g . f = Sc $ unSc g . unSc f

instance MonadPlus m => Format (Sc m String) where
  lit s = liftSc $ \inp k -> liftM (flip (,) k) $ mp $ stripPrefix s inp
  char  = liftSc $ \inp k -> liftM (second' k)  $ mp $ split1        inp
  int   = liftSc $ \inp k -> liftM (swap . first (k . read)) $ satisfies isDigit inp
  pp p  = liftSc $ \inp k -> liftM (swap . first  k) $ mp $ ppParse p inp

mp :: MonadPlus m => Maybe a -> m a
mp = maybe mzero return

split1 :: [a] -> Maybe ([a],a)
split1 as = case as of
  a : as' -> Just (as',a)
  _       -> Nothing

satisfies :: MonadPlus m => (a -> Bool) -> [a] -> m ([a],[a])
satisfies pr as = return ([],as) `mplus` case as of
  a : as'
    | pr a -> liftM (first (a :)) $ satisfies pr as'
  _        -> mzero

instance MonadPlus m => PostFormat (Sc m String) where
  post p f = liftSc $ \inp k -> runSc p inp $ k . f

-- }}}

-- F Category / Profunctor / Semigroupoid {{{

newtype F f p a b = F
  { unF :: p (f a) (f b)
  }

instance (Functor f, Profunctor p) => Profunctor (F f p) where
  dimap f g = F . dimap (fmap f) (fmap g) . unF

instance Semigroupoid p => Semigroupoid (F f p) where
  g `o` f = F $ unF g `o` unF f

instance Category p => Category (F f p) where
  id    = F id
  g . f = F $ unF g . unF f

-- }}}

-- Examples {{{

f0 :: Fmt '[] a
f0  = lit "Fool!"

f1 :: Fmt '[Char] a
f1  = char

f2 :: Fmt '[Char,Int] a
f2  = lit "Hello, " . char . lit "! It's your " . int . lit " birthday."

{-
f3 :: forall a. Pre '[Int] a
f3 = lit "It's your " . pre int go . lit " birthday."
  where
  go :: (Int -> a) -> Int -> a
  go k i = case i of
    
-}

-- }}}

-- Params {{{

data Params (r :: *) (as :: [*]) where
  ID :: Params r '[]
  P  :: (a -> Params r as) -> Params r (a ': as)

type family (as :: [*]) ++ (bs :: [*]) :: [*] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

(<+>) :: Params r as -> Params r bs -> Params r (as ++ bs)
as <+> bs = case as of
  ID  -> bs
  P f -> P $ \a -> f a <+> bs
infixr 4 <+>

-- }}}

-- Util {{{

single :: [a] -> Maybe a
single as = case as of
  [a] -> Just a
  _   -> Nothing

-- }}}

