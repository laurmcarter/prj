{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Unify.CKanren where

import Unify

import Unify.MKTypes

import Control.Monad.State
import Data.Maybe
import Data.List
import qualified Data.Foldable as F
import qualified Data.Traversable as T

type CK = LP LV '[Cons] '[Int,Bool,Char,String,()]

-- LV {{{

type Index = Int

data LV a = LV Index deriving (Show,Functor,F.Foldable,T.Traversable)

instance Equal LV where
  equal (LV x) (LV y) = x == y

instance Render LV where
  render (LV x) = "_." ++ show x

instance Uni LV LV where
  occ (LV x) (LV y) = return (x == y)

instance LogicVar LV where
  data Package LV f = Package
    { index :: Index
    , substitution :: Subst (Mu f)
    , constraints :: [Constraint LV f]
    } deriving Show
  initPkg = Package { index = 0, substitution = emptyS, constraints = emptyC }
  varBehavior uvar v = updateS (inject uvar) v
  newVar = do
    x <- getIndex
    modifyIndex succ
    return $ LV x
  getSubst = gets substitution
  putSubst s' = modify $ \s -> s { substitution = s' }

-- }}}

-- Constraint {{{

data Constraint v f = Constraint
  { name :: String
  , rands :: [Mu f]
  , proc :: Goal v f
  }

instance (Equal f) => Eq (Constraint LV f) where
  (Constraint n1 xs1 _) == (Constraint n2 xs2 _) = n1 == n2 && xs1 == xs2

instance (Render f) => Show (Constraint LV f) where
  show (Constraint n xs _) = "(" ++ n ++ intercalate " " (map show xs) ++ ")"

-- }}}

-- Helpers {{{

anyVar :: Unifiable f LV => [Mu f] -> Bool
anyVar = any $ isJust . isVar

anyRelevantVar :: Unifiable f LV => [Mu f] -> [Mu f] -> Bool
anyRelevantVar ts xs = any (`elem` xs) ts

prefixS :: (Unifiable f LV) => Subst (Mu f) -> Subst (Mu f) -> Subst (Mu f)
prefixS s s' = if s `isSuffixOf` s'
  then emptyS
  else head s' : prefixS s (tail s')

getConstraints :: (Unifiable f LV) => GoalM LV f [Constraint LV f]
getConstraints = gets constraints
  
getIndex :: (Unifiable f LV) => GoalM LV f Index
getIndex = gets index

modifyC :: (Unifiable f LV)
  => ([Constraint LV f] -> [Constraint LV f]) -> Goal LV f
modifyC f = modify $ \p -> p { constraints = f $ constraints p }

modifyIndex :: (Unifiable f LV) => (Index -> Index) -> Goal LV f
modifyIndex f = modify $ \p -> p { index = f $ index p }

-- }}}

-- Substitution {{{

updateS :: (Unifiable f LV) => Mu f -> Mu f -> Goal LV f
updateS x v = do
  runConstraints $
    if isJust $ isVar v
      then [x,v]
      else [x]
  extendS x v

-- }}}

-- Constraint Store {{{

emptyC = []

extendC :: (Unifiable f LV) => Constraint LV f -> Goal LV f
extendC oc = modifyC (oc:)

updateC :: (Unifiable f LV) => Constraint LV f -> Goal LV f
updateC oc = if anyVar $ rands oc
  then extendC oc
  else succeed

-- }}}

-- Fixed Point {{{

runConstraints :: (Unifiable f LV)
  => [Mu f] -> Goal LV f
runConstraints xs = getConstraints >>= mapM_ f
  where
  f oc = if anyRelevantVar (rands oc) xs
    then remRun oc
    else succeed

remRun :: (Unifiable f LV) => Constraint LV f -> Goal LV f
remRun oc = modifyC (delete oc) >> proc oc

-- }}}

test1 :: Results CK
test1 = run Nothing $ \q ->
  q === int 1

test2 :: Results CK
test2 = run Nothing $ \q ->
  fresh $ \x y -> do
    x === int 2
    y === int 3
    q === cons x y

