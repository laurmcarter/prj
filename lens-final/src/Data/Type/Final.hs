{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Data.Type.Final
  ( module Data.Type.Final
  , module Prelude
  ) where

import Prelude hiding
  ( Eq((==))
  , Ord(compare)
  , Show(showsPrec,show)
  , Read(readsPrec)
  )
import qualified Prelude as P
import Data.Monoid hiding (Product(..))
import Control.Applicative
import Control.Arrow (first,second)
import Data.List (intersperse)
import Data.Foldable (Foldable)
import qualified Data.Foldable as F

newtype Eq r = Eq
  { (==) :: r -> Bool
  }

newtype Ord r = Ord
  { compare :: (r,r -> Ordering)
  }

newtype Show = Show
  { showsPrec :: Int -> ShowS
  }

show :: Show -> String
show s = showsPrec s 0 ""

instance P.Show Show where
  showsPrec d (Show f) = f d

newtype Pretty u a = Pretty
  { prettysPrec :: (u,Int,String) -> ((u,Int,String),a)
  }

instance Functor (Pretty u) where
  fmap f (Pretty m) = Pretty $ second f . m

instance Applicative (Pretty u) where
  pure a = Pretty (,a)
  Pretty mf <*> Pretty mx = Pretty $ \s0 ->
    let (s1,f) = mf s0
        (s2,x) = mx s1
    in (s2,f x)

instance Monad (Pretty u) where
  return = pure
  Pretty m >>= f = Pretty $ \s0 ->
    let (s1,a) = m s0
    in prettysPrec (f a) s1

pretty :: u -> Pretty u () -> String
pretty u (Pretty m) = _3 $ fst $ m (u,0,"")

pretty_ :: Pretty () () -> String
pretty_ = pretty ()

str :: String -> Pretty u ()
str s = Pretty $ \(us,prec,rest) -> ((us,prec,rest ++ s),())

chr :: Char -> Pretty u ()
chr = str . (:[])

space :: Pretty u ()
space = chr ' '

wrds :: [Pretty u ()] -> Pretty u ()
wrds = sequence_ . intersperse space

getPrec :: Pretty u Int
getPrec = Pretty $ \s@(_,p,_) -> (s,p)

setPrec :: Int -> Pretty u ()
setPrec p' = Pretty $ \(us,_,rest) -> ((us,p',rest),())

getUser :: Pretty u u
getUser = Pretty $ \s@(u,_,_) -> (s,u)

setUser :: u -> Pretty u ()
setUser u = Pretty $ \(_,p,rest) -> ((u,p,rest),())

parens :: Int -> Pretty u () -> Pretty u ()
parens p m = do
  prec <- getPrec 
  setPrec $ p + 1
  if prec > p
    then do
      chr '('
      m
      chr ')'
    else m

pApp :: [Pretty u ()] -> Pretty u ()
pApp = parens 10 . wrds

prettys :: P.Show a => a -> Pretty u ()
prettys = str . P.show

instance P.Show (u,Pretty u ()) where
  showsPrec d (u,Pretty f) = _3 . fst . f . (u,d,)

newtype Show1 a = Show1
  { showsPrec1 :: P.Show a => Int -> ShowS
  }

show1 :: P.Show a => Show1 a -> String
show1 s = showsPrec1 s 0 ""

newtype Id r = Id
  { getId :: r
  }

data Id1 r a = Id1
  { getId1 :: r a
  }

data Product f g a = Product (f a) (g a)
  deriving (P.Eq,P.Show)

pFst :: Product f g a -> f a
pFst (Product f _) = f

pSnd :: Product f g a -> g a
pSnd (Product _ g) = g

newtype FoldMap m a = FoldMap
  { foldMap :: (a -> m) -> m
  }

_1 :: (a,b,c) -> a
_1 (a,_,_) = a

_2 :: (a,b,c) -> b
_2 (_,b,_) = b

_3 :: (a,b,c) -> c
_3 (_,_,c) = c

newtype Typed r a b = Typed
  { typed :: r a
  }

