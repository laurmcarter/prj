{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Data.Functor.Higher where

import Prelude hiding (id,(.))
import qualified Prelude as Pre
import Control.Category
import Data.Profunctor

newtype (f :: k -> *) :-> (g :: k -> *) = HArr
  { hArr :: forall (a :: k). f a -> g a
  }
infixr 4 :->

class Functor1 (f :: (k -> *) -> k -> *) where
  fmap1 :: (a :-> b) -> f a :-> f b

instance Category (:->) where
  id    = HArr Pre.id
  g . f = HArr $ hArr g . hArr f

data PArr (c :: k -> *) (f :: k) (g :: k) = PArr
  { pArr :: c f -> c g
  }

