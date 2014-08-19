{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unify.Fresh where

import Unify.Fix
import Unify.Goal
import Unify.Unification

import Control.Applicative
import Control.Monad.State

class (Unifiable f v) => Freshable x (v :: * -> *) f a | x -> v f where
  fresh :: x -> GoalM v f a

instance (Unifiable f v) => Freshable (G (Package v f) (Mu f) a) v f a where
  fresh = id

instance (Unifiable f v, Freshable s v f a) => Freshable (Mu f -> s) v f a where
  fresh f = (inject <$> (newVar :: GoalM v f (v (Mu f)))) >>= fresh . f

-- Run abstractions {{{

run :: (Unifiable f v) => Maybe Int -> (Mu f -> Goal v f) -> Results (Mu f)
run rn f = runGoal (Just 10) rn $ fresh $ \x -> (f x >> resolve x)

runS :: (Unifiable f v) => Maybe Int -> (Mu f -> Goal v f) -> Results (Subst (Mu f))
runS rn f = runGoal (Just 10) rn $ fresh $ \x -> (f x >> getSubst)

runE :: (Unifiable f v) => Maybe Int -> Maybe Int -> (Mu f -> Goal v f) -> Results (Mu f)
runE en rn f = runGoal en rn $ fresh $ \x -> (f x >> resolve x)

runSE :: (Unifiable f v) => Maybe Int -> Maybe Int -> (Mu f -> Goal v f) -> Results (Subst (Mu f))
runSE en rn f = runGoal en rn $ fresh $ \x -> (f x >> getSubst)

runP :: (Unifiable f v) => Maybe Int -> (Mu f -> Goal v f) -> Results (Package v f)
runP rn f = runGoal (Just 10) rn $ fresh $ \x -> (f x >> get)

runGoal :: (Unifiable f v) => Maybe Int -> Maybe Int -> GoalM v f a -> Results a
runGoal en rn g = fromStream en rn $ evalGoal g initPkg

-- }}}

