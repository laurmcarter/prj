{-# LANGUAGE PolyKinds #-}

module Prelude.Indexed
  ( module Prelude.Indexed
  , module Data.Indexed
  , module Prelude
  , module Control.Applicative
  ) where

import qualified Prelude as UnIxed
import Prelude hiding ((>>=),(>>),return)
import qualified Control.Applicative as UnIxed

import Control.Applicative hiding ((<**>),empty)
import Control.Monad
import Data.Indexed

return :: IxPointed m => a -> m i i a
return = ireturn

(>>=) :: IxBind m => m i j a -> (a -> m j k b) -> m i k b
(>>=)  = (>>>=)
infixr 1 >>=

(>>)  :: IxBind m => m i j a -> m j k b -> m i k b
(>>)   = (>>>)
infixl 1 >>

-- Unindexed functions that were overloaded, reprovided for convenience.

ap_ :: Applicative f => f a -> f (a -> b) -> f b
ap_ = (UnIxed.<**>)

empty_ :: Alternative f => f a
empty_ = UnIxed.empty

return_ :: Monad m => a -> m a
return_ = UnIxed.return

bind_ :: Monad m => m a -> (a -> m b) -> m b
bind_ = (UnIxed.>>=)

then_ :: Monad m => m a -> m b -> m b
then_ = (UnIxed.>>)

