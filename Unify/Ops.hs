{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Unify.Ops where

import Unify.Fix
import Unify.Goal
import Unify.Unification

import Control.Monad
import Control.Monad.State

import Debug.Trace

conj :: Unifiable f v => [Goal v f] -> Goal v f
conj = foldl (>>) (return ())
--conj = sequence_   -- foldr

disj :: Unifiable f v => [Goal v f] -> Goal v f
disj = foldl mplus mzero
--disj = msum   -- foldr

anyo :: Unifiable f v => Goal v f -> Goal v f
anyo g = disj [ g , anyo g ]

onceo :: Unifiable f v => Goal v f -> Goal v f
onceo g = G $ StateT $ \s ->
  let (EitherT (Interleave ss)) = runStateT (runG g) s in
    case ss of
      [] -> EitherT $ Interleave []
      a:_ -> EitherT $ Interleave [a]

(===) :: Unifiable f v => Mu f -> Mu f -> Goal v f
(===) = unify

prt :: (Unifiable f v, Show (Package v f)) => Goal v f
prt = do
  s <- get
  trace (show s) succeed

