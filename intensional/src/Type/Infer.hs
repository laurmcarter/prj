{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Type.Infer where

import Type.Context
import Type.Equality
import Type.Type
import Data.Indexed

import Control.Arrow (first)

data Infer (ps :: [Product k l]) (qs :: [Product k l]) r where
  Infer :: (Context ps -> (Presuming ps => (r,Context qs)))
        -> Infer ps qs r

instance IxFunctor Infer where
  imap f (Infer m) = Infer $ first f . m

instance IxPointed Infer where
  ireturn a = Infer $ \c -> (a,c)

instance IxApply Infer where
  iap (Infer (ff :: Context ps -> (Presuming ps => (a -> b,Context qs))))
      (Infer (fa :: Context qs -> (Presuming qs => (a,Context rs))))
    = Infer res
    where
    res :: Presuming ps => Context ps -> (b,Context rs)
    res ps = first f $ qs |- fa qs
      where
      (f,qs) = ff ps

-- inferred: instance IxApplicative Infer

instance IxBind Infer where
  ibind (Infer (m :: Context ps -> (Presuming ps => (a,Context qs))))
        (f :: a -> Infer qs rs b)
    = Infer res
    where
    res :: Presuming ps => Context ps -> (b,Context rs)
    res ps = runInfer (f a) qs
      where
      (a,qs) = m ps

-- inferred: instance IxMonad Infer

runInfer :: Infer ps qs r -> Context ps -> (r,Context qs)
runInfer (Infer m) c = c |- m c

evalInfer :: Infer ps qs r -> Context ps -> r
evalInfer m = fst . runInfer m

execInfer :: Infer ps qs r -> Context ps -> Context qs
execInfer m = snd . runInfer m

justify :: Infer ps qs r -> Context ps -> r
justify = evalInfer

proof :: Infer ps qs r -> Context ps -> Context qs
proof = execInfer

