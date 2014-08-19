{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module UnifyTerm.Set where

import UnifyTerm
import UnifyTerm.MiniKanren
import UnifyTerm.MKTypes

import Control.Applicative
import Control.Monad.State.Class
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Control.Lens hiding (Cons (..), cons,set)
import Control.Monad
import Data.List
import Data.Maybe

type SET = LP LV '[Cons,Set] '[Int,Bool,Char,String,()]

-- Set {{{

data Set a = Set [a] deriving (Show,Functor,F.Foldable,T.Traversable)

instance Render Set where
  render (Set xs) = "{" ++ (intercalate "," $ map show xs) ++ "}"

instance Equal Set where
  equal (Set xs) (Set ys) = and [ or [ x == y | y <- ys ] | x <- xs ]

-- FIXME: remove overlapping cases
instance Uni LV Set where
  uni s (Set xs) (Set ys) =
    let gx = filter (not . isJust . var) xs
        gy = filter (not . isJust . var) ys in do
      let gxs = [ disj [ unify s x y | y <- ys ] | x <- gx ]
          gys = [ disj [ unify s y x | x <- xs ] | y <- gy ]
        in

      conj [ conj [ disj [ unify s x y | y <- ys ] | x <- gx ]
           , conj [ disj [ unify s y x | x <- xs ] | y <- gy ]
           ]
  res s (Set xs) = do
    xs' <- mapM (resolve s) xs
    return (Set $ nub xs')

set :: (Set :<: f, Unifiable v f) => [Term v f] -> Term v f
set = inj . Set

{-
introSet :: (Render f, Unifiable f v, Set :<: f) => [Term v f] -> GoalM v f (Term v f)
introSet xs = do
  xs' <- mapM resolve xs
  let vs = filter (isJust . var) xs'
  msum (return (inject $ Set xs') :
        [ do unify v x
             xsFinal <- nub <$> mapM resolve xs'
             return (inject $ Set xsFinal)
          | v <- vs, x <- xs', v /= x
        ])
-}

-- }}}

testSET1 :: Results SET
testSET1 = run Nothing $ \q -> do
  fresh $ \x s -> do
    --s <- introSet [int 1, x]
    s === set [int 1, x]
    q === s

testSET2 :: Results SET
testSET2 = run Nothing $ \q -> do
  fresh $ \x s -> do
    --s <- introSet [int 1, x]
    s === set [int 1, x]
    q === int 2

