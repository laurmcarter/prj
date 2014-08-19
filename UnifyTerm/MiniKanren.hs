{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module UnifyTerm.MiniKanren where

import UnifyTerm
import UnifyTerm.MKTypes

import Control.Lens hiding (Cons (..), cons)

import Control.Applicative
import Control.Monad
import Control.Monad.State.Class
import qualified Data.Foldable as F
import qualified Data.Traversable as T

-- LV {{{

type Gen = Int

data LV = LV Gen deriving (Eq)

type MK = LP LV '[Cons] '[Int,Bool,Char,String,()]

instance Show LV where
  show (LV x) = "_." ++ show x

instance LogicVar LV where
  data Package LV f = Package
    { _gen :: Gen
    , _substitution :: Subst LV f
    } deriving (Show)
  subst = substitution
  emptyPkg = Package { _gen = 0, _substitution = emptyS }
  varBehavior _ u v = return [(u,v)]
  newVar = do
    x <- gen <<%= succ
    return $ Var $ LV x

gen :: Lens' (Package LV f) Gen
gen f (Package i s) = (\i' -> Package i' s) <$> (f i)

substitution :: Lens' (Package LV f) (Subst LV f)
substitution f (Package i s) = (\s' -> Package i s') <$> (f s)

-- }}}

(===) :: Unifiable LV f => Relation Two LV f
u === v = do
  s <- use subst
  liftStream (unify s u v) fail (subst <>=)

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

test7 :: Results MK
test7 = run Nothing $ \q ->
  fresh $ \x y z -> do
    disj
      [ do x === int 1
           y === int 2
      , do x === int 3
           y === int 4
      ]
    z === list [x,y]
    q === z

