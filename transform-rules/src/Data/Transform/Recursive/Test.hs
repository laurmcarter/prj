{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Transform.Recursive.Test where

import Control.Applicative
import Control.Lens
import Data.Data.Lens
import Data.Monoid
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Transform.Recursive
-- import Data.Transform.Recursive.TH
import Data.Data (Data)
import Data.Typeable (Typeable)

-- Fix {{{

newtype Fix f = Fix (f (Fix f))

makeLensesWith
  ( isoRules
  & lensIso .~ (Just . ('_' :))
  ) ''Fix

instance Show (f (Fix f)) => Show (Fix f) where
  showsPrec d (Fix m) = showParen (d > 10)
    $ showString "Fix "
    . showsPrec 11 m

instance Monoid (f (Fix f)) => Monoid (Fix f) where
  mempty = Fix mempty
  mappend (Fix m) (Fix n) = Fix $ m <> n

instance Traversable f => Plated (Fix f) where
  plate = from _Fix . traversed

-- }}}

-- Expr {{{

data Expr
  = I Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq,Show,Typeable,Data)

makePrisms ''Expr

instance Num Expr where
  (+)         = Add
  (*)         = Mul
  (-)         = undefined
  abs         = undefined
  signum      = undefined
  fromInteger = I . fromInteger

instance Plated Expr where
  plate = _Add . both <+> _Mul . both

newtype Foo = Foo
  { foo :: Expr
  } deriving (Eq,Show)

makeLensesWith
  ( isoRules
  & lensIso .~ (Just . ('_' :))
  ) ''Foo

newtype Bar a = Bar
  { bar :: (a,Expr)
  } deriving (Eq,Show)

makeLensesWith
  ( isoRules
  & lensIso .~ (Just . ('_' :))
  ) ''Bar

exprTest :: Expr
exprTest = 1 + 4 * 2

-- }}}

-- F {{{

data F a
  = F Int a
  | G Bool
  deriving (Eq,Show,Functor,Foldable,Traversable)

{-
instance Functor F where
  fmap f e = case e of
    F i a -> F i $ f a
    G b   -> G b

instance Foldable F where
  foldMap f e = case e of
    F _ a -> f a
    G _   -> mempty

instance Traversable F where
  traverse f e = case e of
    F i a -> F i <$> f a
    G b   -> pure $ G b
-}

fInt :: Traversal' (F a) Int
fInt f e = case e of
  F i a -> F <$> f i <*> pure a
  G b   -> pure $ G b

fBool :: Traversal' (F a) Bool
fBool f e = case e of
  F i a -> pure $ F i a
  G b   -> G <$> f b

fIntBool :: Traversal (F a) (F a) Int Bool
fIntBool f e = case e of
  F i _ -> G <$> f i
  G b   -> pure $ G b

fBoolInt :: a -> Traversal (F a) (F a) Bool Int
fBoolInt a f e = case e of
  F i a -> pure $ F i a
  G b   -> F <$> f b <*> pure a

fEx :: Fix F
fEx = Fix (F 2 (Fix (G True)))

-- }}}

