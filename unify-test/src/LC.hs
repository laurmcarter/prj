{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module LC where

import Unify
import Unify.Types

import Control.Monad.Unify
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T

data Var a = Var String   deriving (Eq,Show,Functor,Foldable,Traversable)
data Lam a = Lam String a deriving (Eq,Show,Functor,Foldable,Traversable)
data App a = App a a      deriving (Eq,Show,Functor,Foldable,Traversable)

instance Zip Var

instance Zip Lam where
  zipF scLower scHere fl r e1@(Lam x a) e2@(Lam y b)
      = scLower a b r
    >>= return . scHere e1 e2

instance Zip App where
  zipF scLower scHere fl r e1@(App a b) e2@(App c d)
      = scLower a c r
    >>= scLower b d
    >>= return . scHere e1 e2

type Term f = Var :+: Lam :+: App :+: f



data TInt a = TInt     deriving (Eq,Show,Functor,Foldable,Traversable)
data TArr a = TArr a a deriving (Eq,Show,Functor,Foldable,Traversable)

instance Zip TInt

instance Zip TArr where
  zipF scLower scHere fl r e1@(TArr a b) e2@(TArr c d)
      = scLower a c r
    >>= scLower b d
    >>= return . scHere e1 e2

type Type f = TInt :+: TArr :+: f

