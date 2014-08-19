{-# LANGUAGE DeriveFunctor,DeriveFoldable,DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module UnifyTerm.Unification where

import UnifyTerm.Goal
import Control.Applicative
  ( Applicative (..)
  , Const (..)
  , getConst
  , (<$>)
  )

import Control.Lens
import Control.Lens.Internal.Instances ()
import Control.Monad (MonadPlus (..),msum)
import Control.Monad.State (gets, modify)
import qualified Data.Foldable as F
import qualified Data.Traversable as T

-- Types {{{

type Results a = Either [String] [a]

type Goal v f = GoalM v f ()
type GoalM v f = G (Package v f)

data Term v f
  = Var v
  | Term (f (Term v f))

type Subst v f = [(v,Term v f)]
type Prefix v f = Subst v f

-- }}}

-- Base {{{

data (f :+: g) e = Inl (f e) | Inr (g e) deriving (Show,Functor,F.Foldable,T.Traversable)

infixr :+:

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance (Functor f) => f :<: f where
  inj = id
  prj = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  prj cp = case cp of
    Inl e -> Just e
    Inr _ -> Nothing

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  prj cp = case cp of
    Inl _ -> Nothing
    Inr e -> prj e

-- }}}

-- Equal {{{

class (Functor f) => Equal f where
  equal :: (Eq v, Equal g) => f (Term v g) -> f (Term v g) -> Bool

instance (Equal f, Equal g) => Equal (f :+: g) where
  equal cp1 cp2 = case (cp1,cp2) of
    (Inl e1,Inl e2) -> equal e1 e2
    (Inr e1,Inr e2) -> equal e1 e2
    _               -> False

instance (Eq v, Equal f) => Eq (Term v f) where
  x == y = case (x,y) of
    (Var u,Var v)   -> u == v
    (Term s,Term t) -> s `equal` t
    _               -> False

-- }}}

-- Render {{{

class (Functor f) => Render f where
  render :: (Show v,Render g) => f (Term v g) -> String

instance (Render f, Render g) => Render (f :+: g) where
  render cp = case cp of
    Inl e -> render e
    Inr e -> render e

instance (Show v, Render f) => Show (Term v f) where
  show x = case x of
    Var v  -> show v
    Term t -> render t

-- }}}

-- LogicVar {{{

class (Eq v, Show v) => LogicVar v where
  data Package v :: (* -> *) -> *
  emptyPkg :: Unifiable v f => Package v f
  varBehavior :: Unifiable v f => Subst v f -> v -> Term v f -> Stream (Prefix v f)
  newVar :: Unifiable v f => GoalM v f (Term v f)
  subst :: Unifiable v f => Lens' (Package v f) (Subst v f)

  walk :: (Unifiable v f) => Subst v f -> Term v f -> Term v f
  walk s x = case x of
    Var v  -> maybe x (walk s) $ lookup v s
    Term _ -> x

-- }}}

-- Uni {{{

class (Equal f, Render f, F.Foldable f, T.Traversable f, LogicVar v) => Uni v f | f -> v where
  uni :: Unifiable v g => Subst v g -> f (Term v g) -> f (Term v g) -> Stream (Prefix v g)
  uni _ = bad -- default to failure

  res :: Unifiable v g => Subst v g -> f (Term v g) -> f (Term v g)
  res = fmap . resolve

  occ :: Unifiable v g => Subst v g -> v -> f (Term v g) -> Bool
  occ s = foldOr . occursCheck s

  bad :: Unifiable v g => f (Term v g) -> f (Term v g) -> Stream (Prefix v g) 
  bad x y = fail (render x ++ " =/= " ++ render y)

instance (Uni v f, Uni v g) => Uni v (f :+: g) where
  uni s cp1 cp2 = case (cp1,cp2) of
    (Inl e1,Inl e2) -> uni s e1 e2
    (Inr e1,Inr e2) -> uni s e1 e2
    _               -> bad cp1 cp2

-- }}}

-- Unifiable {{{

class (Uni v f, LogicVar v) => Unifiable v f where
  unify :: Subst v f -> Term v f -> Term v f -> Stream (Prefix v f)
  unify s u v = do
    let u' = walk s u
        v' = walk s v
    if u' == v'
    then return emptyS
    else case (u',v') of
      (Var uvar,_) -> do
        assert (not $ occursCheck s uvar v') $ cycleFail uvar v'
        varBehavior s uvar v'
      (_,Var vvar) -> do
        assert (not $ occursCheck s vvar u') $ cycleFail vvar u'
        varBehavior s vvar u'
      (Term ut,Term vt) -> uni s ut vt

  resolve :: Subst v f -> Term v f -> Term v f
  resolve s u = let u' = walk s u in
    case u' of
      Var _  -> u'
      Term t -> Term $ res s t

  occursCheck :: Subst v f -> v -> Term v f -> Bool
  occursCheck s uvar v = let v' = walk s v in
    case v' of
      Var vvar -> uvar == vvar
      Term t   -> occ s uvar t

instance (Uni v f) => Unifiable v f

-- }}}

-- Substitutions {{{

emptyS :: (Unifiable v f) => Subst v f
emptyS = []

extendS :: (Unifiable v f) => Subst v f -> v -> Term v f -> Stream (Prefix v f)
extendS s u v = return ((u,v):s)

-- }}}

-- Helpers {{{

assert :: MonadPlus m => Bool -> String -> m ()
assert p err = if p then return () else fail err

succeed :: (LogicVar v) => Goal v f
succeed = return ()

foldOrM :: (F.Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
foldOrM f = F.foldrM (\a n -> f a >>= return . (||) n) False

foldOr :: (F.Foldable t) => (a -> Bool) -> t a -> Bool
foldOr f = F.foldr (\a n -> n || f a) False

zipWithMP :: (MonadPlus m, Show a, Show b) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithMP f as bs = case (as,bs) of
  ([],[])       -> return []
  ([],_)        -> fail $ lengthFail as bs
  (_,[])        -> fail $ lengthFail as bs
  (a:as',b:bs') -> do c <- f a b
                      rest <- zipWithMP f as' bs'
                      return (c:rest)

zipWithMP_ :: (MonadPlus m, Show a, Show b) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithMP_ f as bs = zipWithMP f as bs >> return ()

inject :: (f :<: g) => f (Term v g) -> Term v g
inject = Term . inj

match :: (g :<: f) => Term v f -> Maybe (g (Term v f))
match x = case x of
  Var _  -> Nothing
  Term t -> prj t

var :: Term v f -> Maybe v
var x = case x of
  Var v -> Just v
  Term _ -> Nothing

foldTerm :: Functor f => (v -> a) -> (f a -> a) -> Term v f -> a
foldTerm vf tf x = case x of
  Var v -> vf v
  Term t -> tf (fmap (foldTerm vf tf) t)

foldTermM :: (Applicative m, Monad m, T.Traversable f) => (v -> m a) -> (f a -> m a) -> Term v f -> m a
foldTermM vf tf x = case x of
  Var v -> vf v
  Term t -> T.traverse (foldTermM vf tf) t >>= tf

cycleFail :: (Unifiable v f) => v -> Term v f -> String
cycleFail uvar v = "Unifying "++show uvar++" and "++show v++" creates an infinite cycle."

lengthFail :: (Show a, Show b) => a -> b -> String
lengthFail as bs = "Length mismatch: "++show as++" and "++show bs

newS :: (Unifiable v f) => Subst v f -> Subst v f -> Subst v f
newS = (++)

liftStream :: (MonadPlus m) => Stream a -> (String -> m b) -> (a -> m b) -> m b
liftStream s ff sf = msum $ fmap mkGoal $ streamToList s
  where
  mkGoal e = case e of
    Left err -> ff err
    Right a  -> sf a

-- }}}

-- Const {{{

instance (Show v) => Render (Const v) where
  render = show . getConst

instance (Eq v) => Equal (Const v) where
  equal (Const x) (Const y) = x == y

{-
deriving instance F.Foldable (Const v)
deriving instance T.Traversable (Const v)
-}

instance (Eq a, Show a, LogicVar v) => Uni v (Const a)

-- }}}

-- Type Families {{{

data Nat = Ze | Sc Nat
type One = Sc Ze
type Two = Sc (Sc Ze)
type Three = Sc (Sc (Sc Ze))

type family Relation (n :: Nat) v (f :: * -> *)
type instance Relation Ze v f = Goal v f
type instance Relation (Sc n) v f = Term v f -> Relation n v f

type family LP v (fs :: [* -> *]) (cs :: [*])
type instance LP lv '[] (c ': cs) = Term lv (LPCs cs (Const c))
type instance LP lv (f ': fs) '[] = Term lv (LPFs fs f)
type instance LP lv (f ': fs) (c ': cs) = Term lv (LPCs cs (LPFs fs (Const c :+: f)))

type family LPCs (cs :: [*]) (k :: * -> *) :: * -> *
type instance LPCs '[] k = k
type instance LPCs (c' ': cs) k = Const c' :+: (LPCs cs k)

type family LPFs (fs :: [* -> *]) (k :: * -> *) :: * -> *
type instance LPFs '[] k = k
type instance LPFs (f' ': fs) k = f' :+: (LPFs fs k)

-- }}}

