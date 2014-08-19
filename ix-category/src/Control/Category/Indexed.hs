{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}

module Control.Category.Indexed where

import Control.Category
import Prelude hiding (id,(.))
import qualified Prelude as P

newtype ((f :: k -> *) :-> (g :: k -> *)) (i :: k) = IxArr
  { ixArr :: f i -> g i
  }

newtype ((f :: k -> *) :*: (g :: k -> *)) (i :: k) = IxProd
  { ixProd :: (f i, g i)
  }

newtype ((f :: k -> *) :+: (g :: k -> *)) (i :: k) = IxSum
  { ixSum :: Either (f i) (g i)
  }

type Q (t :: k -> *) = forall (i :: k). t i

class Functor1 (f :: (k -> *) -> k -> *) where
  fmap1 :: Q (a :-> b) -> Q (f a :-> f b)

class Monad1 (f :: (k -> *) -> k -> *) where
  return1 :: Q (a :-> f a)
  bind1   :: Q (a :-> f b) -> Q (f a :-> f b)

class Category1 (f :: k -> k -> k) where
  type Arr f :: k -> k -> *
  id1 :: Arr f a a
  trans1 :: Arr f b c -> Arr f a b -> Arr f a c

instance Category1 (->) where
  type Arr (->) = (->)
  id1 = id
  trans1 = (.)

instance Category1 (:->) where
  type Arr (:->) = QArr

type family QArr (a :: k -> *) (b :: k -> *) :: * where
  QArr a b = Q (a :-> b)

