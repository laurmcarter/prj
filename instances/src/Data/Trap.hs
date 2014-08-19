{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Data.Trap where

import Data.Maybe (fromMaybe)
import Data.Proxy
import GHC.Prim (Constraint)

data Dict c where
  Dict :: c => Dict c

data Dict1 c a where
  Dict1 :: c a => Dict1 c a

{-
data Trap2 c a b where
  Trap2 :: c a b => Trap2 c a b
-}

-- Member {{{

class (a :: k) ∈ (as :: [k]) where
  evidence :: Evidence a as

instance a ∈ (a ': as) where
  evidence = Head

instance (a ∈ as) => a ∈ (b ': as) where
  evidence = Tail

data Evidence (a :: k) (as :: [k]) where
  Head :: Evidence a (a ': as)
  Tail :: (a ∈ as) => Evidence a (b ': as)

refute :: Evidence a '[] -> b
refute x = seq x $ error "impossible!"

-- }}}

-- All {{{

class All (c :: k -> Constraint) (as :: [k]) where
  withElem :: (b ∈ as) => Proxy as -> (Dict1 c b -> r) -> r

instance All c '[] where
  withElem _ (f :: Dict1 c b -> r) = refute (evidence :: Evidence b '[])

instance (c a, All c as) => All c (a ': as) where
  withElem _ (f :: Dict1 c b -> r) =
    case evidence :: Evidence b (a ': as) of
      Head -> f Dict1
      Tail -> withElem (Proxy :: Proxy as) f

-- }}}

-- Optional {{{

class (p :: Bool) ? (c :: Constraint) where
  given :: Proxy p -> (Dict c -> a) -> Maybe a

instance c => True ? c where
  given _ f = Just $ f Dict

instance False ? c where
  given _ _ = Nothing

class OptTag (tag :: k -> l) (c :: k -> Constraint) (t :: l) where
  givenTag :: Proxy t -> (Dict (t ~ tag a) -> Dict1 c a -> b) -> Maybe b

{-
instance (c a) => OptTag tag c (tag a) where
  givenTag (_ :: Proxy (tag a)) (f :: Dict (t ~ tag a) -> Dict1 c a -> b) = undefined
-}

type family Match (x :: k) (y :: k) :: Bool where
  Match x x = True
  Match x y = False

-- }}}

type family Not (a :: Bool) :: Bool where
  Not True  = False
  Not False = True

class IsFunction (p :: Bool) a
instance (p ~  True) => IsFunction p (c -> d)
instance (p ~ False) => IsFunction p a

class TestFunction a where
  test :: a -> String

instance (IsFunction p a, Not p ? (Show a)) => TestFunction a where
  test (a :: a) = fromMaybe "<function>"
    $ given (Proxy :: Proxy (Not p))
    $ \(Dict :: Dict (Show a)) -> show a

data N
  = Z
  | S N
  deriving (Eq,Show)

