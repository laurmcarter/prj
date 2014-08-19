{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Unify.MiniKanren where

import Unify

import Unify.MKTypes

import Control.Applicative
import Control.Monad.State.Class
import qualified Data.Foldable as F
import qualified Data.Traversable as T

type MK = LP LV '[Cons] '[Int,Bool,Char,String,()]

-- LV {{{

type Index = Int

data LV a = LV Index deriving (Show,Functor,F.Foldable,T.Traversable)

instance Equal LV where
  equal (LV x) (LV y) = x == y

instance Render LV where
  render (LV x) = "_." ++ show x

instance LogicVar LV where
  data Package LV f = Package
    { index :: Index
    , substitution :: Subst (Mu f)
    } deriving (Show)
  initPkg = Package { index = 0, substitution = emptyS }
  varBehavior uvar v = extendS (inject uvar) v
  newVar = do
    x <- getGen
    modifyGen succ
    return $ LV x
  getSubst = gets substitution
  putSubst s' = modify $ \s -> s { substitution = s' }

getGen :: Unifiable f LV => GoalM LV f Index
getGen = gets index

modifyGen :: Unifiable f LV => (Index -> Index) -> Goal LV f
modifyGen f = modify $ \s -> s { index = f $ index s }

instance Uni LV LV where
  occ (LV x) (LV y) = return (x == y)

-- }}}

test1 :: Results MK
test1 = run Nothing $ \q ->
  q === int 1

test2 :: Results MK
test2 = run Nothing $ \q ->
  q === char 'b'

test3 :: Results MK
test3 = run Nothing $ \q ->
  q === bool True

test4 :: Results MK
test4 = run Nothing $ \q ->
  q === cons (int 1) (int 2)

test5 :: Results MK
test5 = run Nothing $ \q ->
  q === list [int 1, int 2, int 3, int 4, int 5]

test6 :: Results MK
test6 = run Nothing $ \q ->
  fresh $ \x y -> do
    disj
      [ do x === int 1
           y === int 2
      , do x === int 3
           y === int 4
      ]
    q === cons x y

