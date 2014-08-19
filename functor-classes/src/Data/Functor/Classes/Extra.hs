{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Functor.Classes.Extra
  ( module Data.Functor.Classes.Extra
  , type (:~:)
  , Constraint
  , Void
  ) where

import Data.Functor.Compose
import Data.Void
import GHC.Exts (Constraint)
import Data.Type.Equality

data Some t = forall a. Some (t a)
data Where (c :: k -> Constraint) (t :: k -> *) =
  forall (a :: k). c a => Where (t a)

type a :+: b = Either a b
infixr 4 :+:
type a :*: b = (a,b)
infixr 5 :*:
type f :.: g = Compose f g
infixr 6 :.:

type family From (t :: k -> *) (as :: [k]) :: * where
  From t '[]  = Void
  From t '[a] = t a
  From t (a ': a' ': as) = t a :+: From t (a' ': as)

newtype Poss (t :: k -> *) (as :: [k]) = Poss
  { poss :: From t as
  }

type family From2 (t :: k -> l -> *) (as :: [(k,l)]) :: * where
  From2 t '[]         = Void
  From2 t '[ '(a,b) ] = t a b
  From2 t ( '(a,b) ': '(c,d) ': as) = t a b :+: From2 t ('(c,d) ': as)

newtype Poss2 (t :: k -> l -> *) (as :: [(k,l)]) = Poss2
  { poss2 :: From2 t as
  }

