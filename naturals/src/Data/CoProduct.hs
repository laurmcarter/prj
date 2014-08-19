{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.CoProduct where

import Data.Nat
import Data.Proxy
import Data.Proxy.TH

import Prelude hiding (fst,snd,lookup,(++))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

-- Product {{{

data Prod a b = a :*: b
  deriving (Eq,Show)

infixr 4 :*:

type family Fst (p :: Prod k l) :: k where
  Fst (a :*: b) = a

type family Snd (p :: Prod k l) :: l where
  Snd (a :*: b) = b

-- }}}

-- Coproduct {{{

data Coprod a b
  = L a
  | R b
  deriving (Eq,Show)

type family Left (p :: Coprod k l) :: Maybe k where
  Left (L a) = Just a
  Left (R b) = Nothing

type family Right (p :: Coprod k l) :: Maybe l where
  Right (L a) = Nothing
  Right (R b) = Just b

-- }}}

[proxify|
  fst     :: p  -> Fst p
  snd     :: p  -> Snd p
  first   :: (a -> b) -> (a :*: c) -> (b :*: c)
  second  :: (a -> b) -> (c :*: a) -> (c :*: b)
  (&&&)   :: (a -> b) -> (a -> c)  ->        a  -> (b :*: c)
  (***)   :: (a -> c) -> (b -> d)  -> (a :*: b) -> (c :*: d)

  isLeft  :: p  -> Left p
  isRight :: p  -> Right p

|]

left    :: (Proxy (a :: k) -> Proxy (b :: l)) -> Proxy (p :: Coprod k m) -> Proxy (q :: Coprod l m)
left _ _ = Proxy

right   :: (Proxy (a :: k) -> Proxy (b :: l)) -> Proxy (p :: Coprod m k) -> Proxy (q :: Coprod m l)
right _ _ = Proxy

