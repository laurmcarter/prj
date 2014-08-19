{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UnifyTerm.Fresh where

import UnifyTerm.Goal
import UnifyTerm.Unification

import Control.Lens
import Control.Applicative
import Control.Monad.State

class (Unifiable v f) => Freshable x v f a | x -> v f where
  fresh :: x -> GoalM v f a

--instance (Unifiable v f) => Freshable (G (Package v f) (Term v f) a) v f a where
instance (Unifiable v f) => Freshable (G (Package v f) a) v f a where
  fresh = id

instance (Unifiable v f, Freshable s v f a) => Freshable (Term v f -> s) v f a where
  fresh f = newVar >>= fresh . f

-- Run abstractions {{{

run :: (Unifiable v f) => Maybe Int -> (Relation One v f) -> Results (Term v f)
run rn f = runWith (Just 10) rn $ fresh $ \x -> do
  f x
  resolve <$> use subst <*> pure x

runSubst :: (Unifiable v f) => Maybe Int -> (Relation One v f) -> Results (Subst v f)
runSubst rn f = runWith (Just 10) rn $ fresh $ \x -> (f x >> use subst)

runE :: (Unifiable v f) => Maybe Int -> Maybe Int -> (Relation One v f) -> Results (Term v f)
runE en rn f = runWith en rn $ fresh $ \x -> do
  f x
  resolve <$> use subst <*> pure x

runSubstE :: (Unifiable v f) => Maybe Int -> Maybe Int -> (Relation One v f) -> Results (Subst v f)
runSubstE en rn f = runWith en rn $ fresh $ \x -> (f x >> use subst)

runPkg :: (Unifiable v f) => Maybe Int -> (Relation One v f) -> Results (Package v f)
runPkg rn f = runWith (Just 10) rn $ fresh $ \x -> (f x >> get)

runWith :: (Unifiable v f) => Maybe Int -> Maybe Int -> GoalM v f a -> Results a
runWith en rn g = fromStream en rn $ evalGoal g emptyPkg

-- }}}

