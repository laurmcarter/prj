{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Constraint.Member where

import Data.Constraint.Dict
import Data.Constraint.Evidence
import Data.Constraint.Optional

import GHC.Prim (Constraint)
import Data.Proxy

type a ∈ as = Member as a

instance Evidence2 Member where
  data Ev2 Member as a where
    Head' :: Ev2 Member (a ': as) a
    Tail' :: Member as a => Proxy as -> Ev2 Member (b ': as) a
  expl2 = undefined
  impl2 = undefined

{-
  impl2 e = case e of
    Head'   -> Dict2
    Tail' _ -> Dict2
-}

data Evidence (as :: [k]) (a :: k) where
  Head ::                Evidence (a ': as) a
  Tail :: Member as a => Evidence (b ': as) a

class Member (as :: [k]) (a :: k) where
  evidence :: Evidence as a

instance Member (a ': as) a where
  evidence = Head

instance Member as a => Member (b ': as) a where
  evidence = Tail

class Equal a b (p :: Bool) | a b -> p

instance a ~ b     => Equal a b True
instance p ~ False => Equal a b p

class Member' (as :: [k]) (a :: k) (p :: Bool) | as a -> p where
  ev :: Maybe (Evidence as a)

instance Member' '[] a False where
  ev = Nothing

{-
instance
  ( Or (Equal a b) (Member' as a) p
  ) => Member' (b ': as) a p where
-}

