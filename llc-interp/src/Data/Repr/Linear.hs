{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Repr.Linear where

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.IO.Class
import Data.Proxy
import GHC.Exts (Constraint)

data F a = Free a deriving (Eq,Show)
data U   = Used   deriving (Eq,Show)

class Lin r where
  z    :: r (F a,h) (U,h) a
  s    :: r hi ho a         -> r (x,hi) (x,ho) a
  (@:) :: r hi h (a -> b)   -> r h ho a              -> r hi ho b

class LinLam r hi ho where
  llam :: r (F a,hi) (U,ho) b -> r hi ho (a -> b)

-- Env transition {{{

class HTrans hi ho where
  trans :: hi -> ho

instance HTrans () () where
  trans = id

instance HTrans hi ho => HTrans (F a,hi) (F a,ho) where
  trans = fmap trans

instance HTrans hi ho => HTrans (U,hi) (U,ho) where
  trans = fmap trans

instance HTrans hi ho => HTrans (F a,hi) (U,ho) where
  trans = fmap trans . consume_

consume :: (F a,h) -> (a,(U,h))
consume (Free a,h) = (a,(Used,h))

consume_ :: (F a,h) -> (U,h)
consume_ = snd . consume

-- }}}

newtype T hi ho a = T { runT :: hi -> (a,ho) }

evalT :: T hi ho a -> hi -> a
evalT m = fst . runT m

eval :: T () () a -> a
eval = flip evalT ()

instance Lin T where
  z      = T consume
  s v    = T $ \(x,hi) -> fmap ((,) x) $ runT v hi
  f @: x = T $ \hi -> 
    let (f',h ) = runT f hi
        (x',ho) = runT x h
    in  (f' x',ho)

instance HTrans hi ho => LinLam T hi ho where
  llam body = T $ f &&& trans
    where
    f h = evalT body . flip (,) h . Free

type Use  r x h = LinLam r (F x,h) (U,h)
type Refl r   h = LinLam r      h     h

t0 ::
  ( Lin  r
  , Use  r (a -> b) h
  , Refl r h
  ) => r h h ((a -> b) -> a -> b)
t0 = llam $ llam $ s z @: z

