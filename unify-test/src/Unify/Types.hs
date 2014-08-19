{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Unify.Types where

import Control.Applicative
import Control.Lens
import Control.Monad.Error
import Data.Monoid
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T

class Zip f where
  -- bottom up zip (?)
  zipF :: MonadError e m =>
          (a -> b -> r -> m r)
       -> (f a -> f b -> r -> r)
       -- ^ good match
       -> (f a -> f b -> e)
       -- ^ bad match
       -> r
       -- ^ base value
       -> f a -> f b
       -> m r
  zipF _ sc _ r a b = return $ sc a b r

around :: (forall a. f a -> g a) -> (g a -> g b -> c) -> f a -> f b -> c
around f g a b = f a `g` f b

-- Mu {{{

data Mu f = In { out :: f (Mu f) }

makeLensesWith
  (isoRules
  & lensIso .~ (Just . ('_' :))
  ) ''Mu

instance Eq (f (Mu f)) => Eq (Mu f) where
  In t == In u = t == u

instance Show (f (Mu f)) => Show (Mu f) where
  showsPrec d (In f) = showParen (d > 5)
    $ showString "In "
    . showsPrec 11 f

-- }}}

-- Nu {{{

data Nu f = In { out :: f (Nu f) }

makeLensesWith
  (isoRules
  & lensIso .~ (Just . ('_' :))
  ) ''Nu

instance Eq (f (Nu f)) => Eq (Nu f) where
  In t == In u = t == u

instance Show (f (Nu f)) => Show (Nu f) where
  showsPrec d (In f) = showParen (d > 5)
    $ showString "In "
    . showsPrec 11 f

-- }}}

-- :+: {{{

data (f :+: g) a
  = InL (f a)
  | InR (g a)
  deriving (Eq,Show)
infixr 4 :+:

makePrisms ''(:+:)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f s = case s of
    InL e -> InL $ fmap f e
    InR e -> InR $ fmap f e

instance (Foldable f, Foldable g) => Foldable (f :+: g) where
  foldMap f s = case s of
    InL e -> F.foldMap f e
    InR e -> F.foldMap f e

instance (Traversable f, Traversable g) => Traversable (f :+: g) where
  traverse f s = case s of
    InL e -> InL <$> T.traverse f e
    InR e -> InR <$> T.traverse f e

instance (Zip f, Zip g) => Zip (f :+: g) where
  zipF scLower scHere fl r s1 s2 = case (s1,s2) of
    (InL a,InL b) -> finish =<< zipF scLower (InL `around` scHere) (InL `around` fl) r a b
    (InR a,InR b) -> finish =<< zipF scLower (InR `around` scHere) (InR `around` fl) r a b
    _             -> throwError $ fl s1 s2
    where
    finish = return . scHere s1 s2

instance (Error (f a), Error (g a)) => Error ((f :+: g) a) where
  noMsg  = InL noMsg
  strMsg = InL . strMsg

-- }}}

-- V {{{

newtype V a b = V { getV :: a } deriving (Eq,Show)

instance Functor (V a) where
  fmap f (V a) = V a

instance Foldable (V a) where
  foldMap _ _ = mempty

instance Traversable (V a) where
  traverse _ (V a) = pure $ V a

instance Zip (V a)

instance Error a => Error (V a b) where
  noMsg = V noMsg
  strMsg = V . strMsg

makeLensesWith
  (isoRules
  & lensIso .~ (Just . ('_' :))
  ) ''V

-- }}}

