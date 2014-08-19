{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Control.Comonad.Parser where

import Control.Applicative
import Control.Comonad
import Control.Lens hiding ((<.>))
import Control.Lens.Internal.Bazaar
import Control.Lens.Internal.Context
import Control.Monad
import Data.Profunctor.Rep
import Data.Functor.Bind
import Data.Functor.Compose

import Data.Type.List

data MallT (w :: * -> *) (bs :: [*]) (a :: *) where
  Done  :: a -> MallT w '[] a
  Store :: b -> w (MallT w bs (b -> a)) -> MallT w (b ': bs) a

instance Functor w => Functor (MallT w bs) where
  fmap f = \case
    Done  a     -> Done    $ f a
    Store b wba -> Store b $ fmap (fmap (f .)) wba
  {-# INLINE fmap #-}

instance Comonad w => Comonad (MallT w bs) where
  extract = \case
    Done  a     -> a
    Store b wba -> extract $ fmap ($ b) $ extract wba
  {-# INLINE extract #-}

{-
  extend f l = case l of
    Done  a     -> Done $ f l
    Store b wba -> Store b $ extend _ wba
      where
      g = Store b
-}

{-
data Mall b a
  = Done a
  | More (Mall b (b -> a)) b

instance Functor (Mall b) where
  fmap f = \case
    Done a   -> Done $ f a
    More g b -> More (fmap (fmap f) g) b

instance Comonad (Mall b) where
  extract = \case
    Done a   -> a
    More f b -> extract f b
-}

{-
  duplicate f = case f of
    Done a   -> Done f
    More g b -> More _ b
-}

{-
  extend f l = case l of
    Done a   -> Done $ f l
    More b g -> _
      where
      x = f $ fmap ($ b) g
-}

{-
  duplicate l = case l of
    Done a   -> Done $ Done a
    More b f -> More b $ 
-}

{-
instance Applicative (Mall b) where
  pure      = Done
  mf <*> mx = case mf of
    Done f     -> f <$> mx
    More b mf' -> More b $ flip <$> mf' <*> mx
-}



