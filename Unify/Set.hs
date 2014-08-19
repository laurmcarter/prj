{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Unify.Set where

import Unify

import Unify.MKTypes

import Control.Applicative
import Control.Monad.State.Class
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Control.Monad
import Data.List
import Data.Maybe

type SET = LP LV '[Cons,Set] '[Int,Bool,Char,String,()]

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

-- Set {{{

data Set a = Set [a] deriving (Show,Functor,F.Foldable,T.Traversable)

instance Render Set where
  render (Set xs) = "{" ++ (intercalate "," $ map show xs) ++ "}"

instance Equal Set where
  equal (Set xs) (Set ys) = and [ or [ x == y | y <- ys ] | x <- xs ]

-- FIXME: remove overlapping cases
instance Uni Set LV where
  uni (Set xs) (Set ys) =
    let gx = filter (not . isJust . isVar) xs
        gy = filter (not . isJust . isVar) ys in do
      conj [ conj [ disj [ unify x y | y <- ys ] | x <- gx ]
           , conj [ disj [ unify y x | x <- xs ] | y <- gy ]
           ]
  res (Set xs) = do
    xs' <- mapM resolve xs
    return (Set $ nub xs')
      
introSet :: (Render f, Unifiable f v, Set :<: f) => [Mu f] -> GoalM v f (Mu f)
introSet xs = do
  xs' <- mapM resolve xs
  let vs = filter (isJust . isVar) xs'
  msum (return (inject $ Set xs') :
        [ do unify v x
             xsFinal <- nub <$> mapM resolve xs'
             return (inject $ Set xsFinal)
          | v <- vs, x <- xs', v /= x
        ])

-- }}}

testSET1 :: Results SET
testSET1 = run Nothing $ \q -> do
  fresh $ \x -> do
    s <- introSet [int 1, x]
    q === s

testSET2 :: Results SET
testSET2 = run Nothing $ \q -> do
  fresh $ \x -> do
    s <- introSet [int 1, x]
    q === int 2

