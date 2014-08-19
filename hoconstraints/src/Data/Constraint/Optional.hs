{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
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

module Data.Constraint.Optional where

import Data.Constraint.Dict

import GHC.Prim (Constraint)
import Data.Proxy

-- Isomorphic to Maybe
class (p :: Bool) ? (c :: Constraint) where
  given :: Proxy p -> (Dict c -> r) -> Maybe r

instance c => True ? c where
  given _ f = Just $ f Dict

instance False ? c where
  given _ _ = Nothing

-- Isomorphic to Either
class Option (p :: Bool) (t :: Constraint) (f :: Constraint) where
  option :: Proxy p -> (Dict t -> r) -> (Dict f -> s) -> Either s r

instance t => Option True t f where
  option _ (f :: Dict t -> r) _ = Right $ f Dict

instance f => Option False t f where
  option _ _ (f :: Dict f -> r) = Left $ f Dict



class Or (c1 :: Bool -> Constraint) (c2 :: Bool -> Constraint) (p :: Bool) | c1 c2 -> p where
  orC :: Proxy p -> (Dict (c1 True) -> r) -> (Dict (c2 True) -> s) -> (Maybe r, Maybe s)

instance Or2 c1 q c2 r p => Or c1 c2 p where
  orC = orC2 (Proxy :: Proxy q) (Proxy :: Proxy r)



class Or2 (c1 :: Bool -> Constraint) (q :: Bool) (c2 :: Bool -> Constraint) (r :: Bool) (p :: Bool) | c1 -> q, c2 -> r, q r -> p where
  orC2 :: Proxy q -> Proxy r -> Proxy p -> (Dict (c1 True) -> a) -> (Dict (c2 True) -> b) -> (Maybe a, Maybe b)

instance (c1 True, c2 True) => Or2 c1 True c2 True True where
  orC2 _ _ _ f g = (Just $ f Dict,Just $ g Dict)

instance (c1 False, q ~ False, c2 True) => Or2 c1 q c2 True True where
  orC2 _ _ _ _ g = (Nothing,Just $ g Dict)

instance (c1 True, c2 False, r ~ False) => Or2 c1 True c2 r True where
  orC2 _ _ _ f _ = (Just $ f Dict,Nothing)

instance (c1 False, q ~ False, c2 False, r ~ False, p ~ False) => Or2 c1 q c2 r p where
  orC2 _ _ _ _ _ = (Nothing,Nothing)



class Test (c :: Constraint) (p :: Bool) | c -> p where
  testC :: Proxy c -> Maybe (Dict c)

instance Delayed c => Test c True where
  testC p = Just $ delayed p Dict

instance (Entails c (Bool ~ Int), p ~ False) => Test c p where
  testC _ = Nothing

type Delayed c = Entails c c

class Entails (c :: Constraint) (d :: Constraint) | c -> d where
  delayed :: Proxy c -> (c => Dict d) -> Dict d

class Entails' t c d | t c -> d, t d -> c where
  delayed' :: t -> Proxy c -> (c => Dict d) -> Dict d

class Entails'' t c d | t c -> d, t d -> c where
  delayed'' :: t -> Proxy c -> (c => Dict d) -> Dict d

instance Entails' () c d => Entails c d where
  delayed = delayed' ()

instance Entails'' t c d => Entails' t c d where
  delayed' = delayed''

instance c => Entails'' () c c where
  delayed'' _ _ Dict = Dict

