{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Data.Constraint.Member where

import Data.Type.Bool
import Data.Type.Equality

data Member (a :: k) (as' :: [k]) (as :: [k]) where
  Head   :: Member a as  (a ': as)
  NotMem :: Member a '[] '[]
  Tail   :: Member a as' as -> Member a (b ': as') (b ': as)

type family Remove (a :: k) (as :: [k]) :: [k] where
  Remove a '[]       = '[]
  Remove a (b ': as) = If (a == b) (Remove a as) (b ': Remove a as)

data List (f :: k -> *) (as :: [k]) where
  Nil  :: List f '[]
  (:>) :: f a -> List f as -> List f (a ': as)
infixr 4 :>

{-
remove :: TestEquality f => f a -> List f as -> (List f (Remove a as),Member a (Remove a as) as)
remove a as = case as of
  Nil      -> (Nil,NotMem)
  b :> as' -> case testEquality a b of
    Just Refl -> undefined -- let (bs,p) = remove a as' in (bs,Tail p)
    _         -> let (bs,p) = remove a as' in (b :> bs,Tail p)
-}

