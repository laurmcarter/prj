{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Control.Category.Values where

import Control.Arrow
import Control.Category
import Prelude hiding (id,(.),(++),head,tail,init,last,null,length)
import qualified Prelude as P
import Data.Proxy

import Data.Type.Equality
import Data.Type.Bool
import Data.Type.List

instance Category Values where
  id    = Values id
  g . f = Values $ values g . values f

newtype Values as bs = Values
  { values :: List as -> List bs
  }

