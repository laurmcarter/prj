{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Unify.Unification where

import Unify.Fix
import Unify.Goal

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Traversable as T

type Results a = Either [String] [a]

type Goal v f = GoalM v f ()
type GoalM v f = G (Package v f) (Mu f)

-- LogicVar {{{

class (Equal v, Render v, F.Foldable v, T.Traversable v) => LogicVar v where
  data Package v :: (* -> *) -> *
  initPkg :: Unifiable f v => Package v f
  varBehavior :: Unifiable f v => v (Mu f) -> Mu f -> Goal v f
  newVar :: Unifiable f v => GoalM v f (v (Mu f))

  getSubst :: GoalM v f (Subst (Mu f))
  putSubst :: Subst (Mu f) -> Goal v f

  modifySubst :: (Subst (Mu f) -> Subst (Mu f)) -> Goal v f
  modifySubst f = getSubst >>= putSubst . f

  isVar :: Unifiable f v => Mu f -> Maybe (v (Mu f))
  isVar = match

  walk :: Equal f => Mu f -> GoalM v f (Mu f)
  walk x = do
    s <- getSubst
    case lookup x s of
      Just v  -> walk v
      Nothing -> return x

-- }}}

-- Uni {{{

class (Equal f, Render f, F.Foldable f, T.Traversable f, LogicVar v) => Uni f v | f -> v where
  uni :: Unifiable g v => f (Mu g) -> f (Mu g) -> Goal v g
  uni = bad -- default to failure

  res :: Unifiable g v => f (Mu g) -> GoalM v g (f (Mu g))
  res = T.traverse $ resolve

  occ :: Unifiable g v => v (Mu g) -> f (Mu g) -> GoalM v g Bool
  occ u = foldOrM $ occursCheck u

  bad :: Unifiable g v => f (Mu g) -> f (Mu g) -> Goal v g
  bad x y = fail (render x ++ " =/= " ++ render y)

instance (Uni f v, Uni g v) => Uni (f :+: g) v where
  uni cp1 cp2 = case (cp1,cp2) of
    (Inl e1,Inl e2) -> uni e1 e2
    (Inr e1,Inr e2) -> uni e1 e2
    _               -> bad cp1 cp2

-- }}}

-- Unifiable {{{

class (Uni f v, LogicVar v, v :<: f) => Unifiable f v where
  unify       :: Mu f -> Mu f -> Goal v f
  unify u v = do
    u'@(In ue) <- walk u
    v'@(In ve) <- walk v
    mcond $
      [ guard (u' == v')               :|:          succeed
      , (isVar u' :: Maybe (v (Mu f))) :>: \uvar -> do b <- occursCheck uvar v'
                                                       assert (not b)
                                                         ("Unifying "++render uvar++" and "++show v'++"creates an infinite cycle.")
                                                       varBehavior uvar v'
      , (isVar v' :: Maybe (v (Mu f))) :>: \vvar -> do b <- occursCheck vvar u'
                                                       assert (not b)
                                                         ("Unifying "++render vvar++" and "++show u'++"creates an infinite cycle.")
                                                       varBehavior vvar u'
      , Else                            $           uni ue ve
      ]

  resolve    :: Mu f -> GoalM v f (Mu f)
  resolve u = out <$> walk u >>= fmap In . res

  occursCheck ::             v (Mu f) -> Mu f -> GoalM v f Bool
  occursCheck ue v = out <$> walk v >>= occ ue

instance (Uni f v, LogicVar v, v :<: f) => Unifiable f v

-- }}}

-- Substitutions {{{

emptyS :: Subst (Mu f)
emptyS = []

extendS :: (Unifiable f v) => Mu f -> Mu f -> Goal v f
extendS u v = modifySubst ((u,v):)

-- }}}

-- Helpers {{{

assert :: (LogicVar v) => Bool -> String -> Goal v f
assert p err = if p then succeed else fail err

succeed :: (LogicVar v) => Goal v f
succeed = return ()

mcond :: MonadPlus m => [Case m a] -> m a
mcond cs = case cs of
  [] -> (fail "Ran off the end of mcond expression")
  (Else m) : _          -> m
  (Just _  :|: m) : _   -> m
  (Nothing :|: _) : cs' -> mcond cs'
  (Just a  :>: f) : _   -> f a
  (Nothing :>: _) : cs' -> mcond cs'

data Case m a
  = forall b . (Maybe b) :|: m a
  | forall b . (Maybe b) :>: (b -> m a)
  | Else (m a)

foldOrM :: (F.Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
foldOrM f = F.foldrM (\a n -> f a >>= return . (||) n) False

zipWithMP :: (MonadPlus m, Show a, Show b) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithMP f as bs = case (as,bs) of
  ([],[])       -> return []
  ([],_)        -> fail ("Length mismatch: " ++ show as ++ " and " ++ show bs)
  (_,[])        -> fail ("Length mismatch: " ++ show as ++ " and " ++ show bs)
  (a:as',b:bs') -> do c <- f a b
                      rest <- zipWithMP f as' bs'
                      return (c:rest)

zipWithMP_ :: (MonadPlus m, Show a, Show b) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithMP_ f as bs = zipWithMP f as bs >> return ()

-- }}}

-- Const {{{

instance (Show v) => Render (Const v) where
  render = show . getConst

instance (Eq v) => Equal (Const v) where
  equal (Const x) (Const y) = x == y

deriving instance F.Foldable (Const v)
deriving instance T.Traversable (Const v)

instance (Eq a, Show a, LogicVar v) => Uni (Const a) v

-- }}}

