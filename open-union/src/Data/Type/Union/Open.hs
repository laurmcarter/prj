{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Data.Type.Union.Open where

import Control.Applicative
import Data.Functor.Identity
import Data.Typeable

data ((a :: * -> *) :> b)
infixr 1 :>

class    Member (t :: * -> *) r
instance Member t (t :> r)
instance Member t r => Member t (t' :> r)

data Union r v = forall t. (Functor t, Typeable t) => Union (t v)

inj :: (Functor t, Typeable t, Member t r) => t v -> Union r v
inj = Union

prj :: (Typeable t, Member t r) => Union r v -> Maybe (t v)
prj (Union v) = runIdentity <$> gcast1 (Identity v)

{-
class Member t r => SetMember set (t :: * -> *) r | r set -> t
instance SetMember set t r => SetMember set t (t' :> r)
-}

(<?>) :: Maybe a -> a -> a
ma <?> a = case ma of
  Just a' -> a'
  _       -> a
infixl 4 <?>

prjForce :: (Typeable t, Member t r) => Union r v -> (t v -> a) -> a
prjForce u f = f <$> prj u <?> error "prjForce with an invalid type"

unsafeReUnion :: Union r w -> Union t w
unsafeReUnion (Union v) = Union v

decomp :: Typeable t => Union (t :> r) v -> Either (Union r v) (t v)
decomp u = Right <$> prj u <?> Left (unsafeReUnion u)

