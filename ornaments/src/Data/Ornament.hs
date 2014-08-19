{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Ornament where

import Control.Applicative
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Typeable
import Control.Concurrent.MVar
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import System.Mem.StableName

class Traversable (Deref t) => MuRef (t :: *) where
  type Deref t :: * -> *
  mapDeref :: Applicative f => (t -> f u)
           -> t -> f (Deref t u)

type Unique = Int
data Graph e = Graph
  { netlist  :: [(Unique, e Unique)]
  , graphGen :: Unique
  }

instance Show (e Unique) => Show (Graph e) where
  showsPrec d (Graph ps u) = showParen (d > 10)
    $ showString "Graph "
    . showList ps
    . showChar ' '
    . shows u

data List a b
  = Empty
  | Cons a b
  deriving (Eq,Show,Functor,Foldable,Traversable)

instance MuRef [a] where
  type Deref [a] = List a
  mapDeref f l = case l of
    []     -> pure Empty
    x : l' -> Cons x <$> f l'

reifyGraph :: MuRef t => t -> IO (Graph (Deref t))
reifyGraph m = do
  rt1 <- newMVar I.empty
  rt2 <- newMVar []
  uVar <- newMVar 0
  root <- findNodes rt1 rt2 uVar m
  pairs <- readMVar rt2
  return $ Graph pairs root

findNodes :: MuRef t
  => MVar (IntMap [(StableName t,Unique)])
  -> MVar [(Unique,Deref t Unique)]
  -> MVar Unique
  -> t -> IO Unique
findNodes rt1 rt2 uVar !m = do
  st <- makeStableName m
  tab <- takeMVar rt1
  case lookp st tab of
    Just var -> do
      putMVar rt1 tab
      return var
    _        -> do
      var <- newUnique uVar
      putMVar rt1 $ I.insertWith (++)
        (hashStableName st)
        [(st,var)]
        tab
      res <- mapDeref (findNodes rt1 rt2 uVar) m
      tab' <- takeMVar rt2
      putMVar rt2 $ (var,res) : tab'
      return var

newUnique :: MVar Unique -> IO Unique
newUnique var = do
  v <- takeMVar var
  let v' = succ v
  putMVar var v'
  return v'

lookp :: StableName a -> IntMap [(StableName a,b)] -> Maybe b
lookp h tab = I.lookup (hashStableName h) tab >>= lookup h

