{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module UnifyTerm.NEQ where

import UnifyTerm
import UnifyTerm.MKTypes

import Control.Applicative
import Control.Monad
import Control.Monad.State.Class
import Control.Lens hiding (Cons (..), cons)
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
    , _diseqs :: DisEqs LV f
    } deriving (Show)
  emptyPkg = Package { _gen = 0, _substitution = emptyS, _diseqs = emptyC }
  subst = substitution
  varBehavior s u v = return [(u,v)]
  newVar = do
    x <- gen <<%= succ
    return $ Var $ LV x

gen :: Lens' (Package LV f) Gen
gen f (Package i s c) = (\i' -> Package i' s c) <$> (f i)

substitution :: Lens' (Package LV f) (Subst LV f)
substitution f (Package i s c) = (\s' -> Package i s' c) <$> (f s)

diseqs :: Lens' (Package LV f) (DisEqs LV f)
diseqs f (Package i s c) = (\c' -> Package i s c') <$> (f c)

-- }}}

type DisEqs v f = [Subst v f]

emptyC :: (Unifiable LV f) => DisEqs LV f
emptyC = []

(===) :: (Unifiable LV f) => Relation Two LV f
u === v = do
  s  <- use subst
  cs <- use diseqs
  liftStream (unify s u v) fail $ \pf ->
    liftStream (verify (pf ++ s) cs emptyC) fail $ \cs' -> do
      subst <>= pf
      diseqs .= cs'

(=/=) :: (Unifiable LV f) => Relation Two LV f
u =/= v = do
  s  <- use subst
  cs <- use diseqs
  liftStream (unify s u v) (const succeed) $ \c -> if null c
    then fail ("Failed to keep " ++ show u ++ " and " ++ show v ++ " from unifying")
    else diseqs %= (c:)

verify :: (Unifiable LV f) => Subst LV f -> DisEqs LV f -> DisEqs LV f -> Stream (DisEqs LV f)
verify s cs acc = case cs of
  []    -> return acc
  c:cs' -> liftStream (unifyStar s c) (const $ verify s cs' acc) $ \c' -> if null c'
    then fail ("Failed disequality constraint: " ++ show c)
    else verify s cs' (c':acc)
  
unifyStar :: (Unifiable LV f) => Subst LV f -> Subst LV f -> Prefix LV f
unifyStar = F.foldrM (\(v,t) s -> unify s (Var v) t)

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
    q =/= list [int 1, int 2]
    q === z
 
