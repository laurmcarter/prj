{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module UnifyTerm.Ops where

import UnifyTerm.Goal
import UnifyTerm.Unification

import Control.Monad
import Control.Monad.State
import Data.Either (rights)

import Debug.Trace
import Text.Show.Pretty

conj :: (Monad m) => [m ()] -> m ()
--conj :: (Unifiable v f) => [Goal v f] -> Goal v f
conj = foldl (>>) (return ())
--conj = sequence_   -- foldr

disj :: (MonadPlus m) => [m a] -> m a
--disj :: (Unifiable v f) => [Goal v f] -> Goal v f
disj = foldl mplus mzero
--disj = msum   -- foldr

anyo :: (Unifiable v f) => Goal v f -> Goal v f
anyo g = disj [ g , anyo g ]

onceo :: (Unifiable v f) => Goal v f -> Goal v f
onceo g = G $ StateT $ return . head . rights . streamToList . runGoal g
  --let (EitherT (Interleave ss)) = runStateT (runG g) s in
  --  case ss of
  --    [] -> EitherT $ Interleave []
  --    a:_ -> EitherT $ Interleave [a]

prt :: (Unifiable v f, Show (Package v f), Show a) => a -> Goal v f
prt x = do
  s <- get
  trace (ppShow x) succeed
  trace (ppShow s) succeed

