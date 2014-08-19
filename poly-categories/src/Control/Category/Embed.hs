{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Category.Embed where

import qualified Prelude
import Prelude hiding (id,(.))

import Data.Semigroupoid
import Control.Categorical

data Embed (p :: l -> l -> *) (f :: k -> l) (a :: k) (b :: k) where
  Embed :: p (f a) (f b) -> Embed p f a b

