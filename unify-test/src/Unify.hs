{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Unify where

import Unify.Types

import Control.Monad.Unify
import Control.Monad.Trans.Class
import Control.Monad.Error
import Control.Monad.Writer

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens hiding (para)
import Data.Functor.Identity
import Data.Monoid
import Data.Proxy
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import GHC.Prim (Constraint)

-- Optional {{{

data Dict (c :: Constraint) where
  Dict :: c => Dict c

class (p :: Bool) ? (c :: Constraint) where
  given :: Proxy p -> (Dict c -> r) -> Maybe r

instance c => True ? c where
  given _ f = Just $ f Dict

instance False ? c where
  given _ _ = Nothing

type family Not (b :: Bool) :: Bool where
  Not True  = False
  Not False = True

not_ :: Proxy p -> Proxy (Not p)
not_ Proxy = Proxy

-- }}}

-- Folding / Unfolding {{{

para :: Functor f => (f (Mu f,a) -> a) -> Mu f -> a
para f = f . fmap (id &&& para f) . out

apo :: Functor f => (a -> f (Either (Mu f) a)) -> a -> Mu f
apo f = In . fmap (either id $ apo f) . f

cata :: Functor f => (f a -> a) -> Mu f -> a
cata f (In t) = f $ fmap (cata f) t

ana :: Functor f => (a -> f a) -> a -> Mu f
ana f = In . fmap (ana f) . f

-- }}}

-- Eval {{{

nodes :: forall f g. (f :<: g, Foldable g) => Mu g -> [f (Mu g)]
nodes = cataR (:) []

cataM :: (Monad m, Traversable f) => (f a -> m a) -> Mu f -> m a
cataM f (In t) = f =<< T.mapM (cataM f) t

muMap :: Functor f => (f (Mu f) -> f (Mu f)) -> Mu f -> Mu f
muMap f (In t) = In $ fmap (muMap f) $ f t

muMapM :: (Monad m, Traversable f)
  => (f (Mu f) -> m (f (Mu f))) -> Mu f -> m (Mu f)
muMapM f (In t) = return . In =<< T.mapM (muMapM f) =<< f t

injMap :: (f :<: g) => (f (Mu g) -> f (Mu g)) -> Mu g -> Mu g
injMap f (In t) = In $ case prj t of
  Just g -> inj $ f $ fmap (injMap f) g
  _      ->           fmap (injMap f) t

injMapM :: (f :<: g, Monad m, Traversable f, Traversable g)
  => (f (Mu g) -> m (f (Mu g))) -> Mu g -> m (Mu g)
injMapM f (In t) = case prj t of
  Just g -> do
    h <- T.mapM (injMapM f) g
    i <- f h
    return $ inject i
  _      -> do
    u <- T.mapM (injMapM f) t
    return $ In u

alterNodes :: (f :<: g) => (f (Mu g) -> Mu g) -> Mu g -> Mu g
alterNodes f (In t) = case prj t of
  Just g -> f  $ fmap (alterNodes f) g
  _      -> In $ fmap (alterNodes f) t

cataL :: forall f g r. (f :<: g, Foldable g) => (r -> f (Mu g) -> r) -> r -> Mu g -> r
cataL f z m@(In t) = case (prj t :: Maybe (f (Mu g))) of
  Just t' -> go (f z t')
  _       -> go z
  where
  go r = F.foldl (cataL f) r t

cataR :: forall f g r. (f :<: g, Foldable g) => (f (Mu g) -> r -> r) -> r -> Mu g -> r
cataR f z m@(In t) = case prj t of
  Just t' -> f t' rest
  _       -> rest
  where
  rest = F.foldr (flip $ cataR f) z t

eval :: Eval r f => Mu f -> r
eval = cata evalIn

class Functor f => Eval r f where
  evalIn :: f r -> r

instance (Eval r f, Eval r g) => Eval r (f :+: g) where 
  evalIn s = case s of
    InL e -> evalIn e
    InR e -> evalIn e

-- }}}

-- :<: {{{

inject :: (f :<: g) => f (Mu g) -> Mu g
inject = In . inj

match :: (f :<: g) => Mu g -> Maybe (f (Mu g))
match = prj . out

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)
infixr 4 :<: 

instance Functor f => f :<: f where
  inj = id
  prj = Just

instance
  ( Functor f, Functor g, Functor h
  , IsSum p g, InjectSum p f (g :+: h))
  => f :<: (g :+: h) where
  inj = inj' (Proxy :: Proxy p)
  prj = prj' (Proxy :: Proxy p)

class (Functor sub, Functor sup) =>
  InjectSum (p :: Bool) (sub :: * -> *) (sup :: * -> *) where
  inj' :: Proxy p -> sub a -> sup a
  prj' :: Proxy p -> sup a -> Maybe (sub a)

instance (f :<: (g :+: h), Functor i) => InjectSum True f ((g :+: h) :+: i) where
  inj' _ = InL . inj
  prj' _ s = case s of
    InL e -> prj e
    InR _ -> Nothing

instance (Functor f, Functor g) => InjectSum False f (f :+: g) where
  inj' _ = InL
  prj' _ s = case s of
    InL e -> Just e
    InR _ -> Nothing

instance (f :<: h, Functor g) => InjectSum False f (g :+: h) where
  inj' _ = InR . inj
  prj' _ s = case s of
    InL _ -> Nothing
    InR e -> prj e

class IsSum (p :: Bool) (f :: * -> *) | f -> p
instance (True  ~ p) => IsSum p (f :+: g)
instance (False ~ p) => IsSum p f

-- }}}

-- Inj {{{

_Inj :: (f :<: g) => Prism' (Mu g) (f (Mu g))
_Inj = prism' (In . inj) (prj . out)

injecting :: (f :<: g) => AnIso' t (f (Mu g)) -> Prism' (Mu g) t
injecting l = _Inj . from l

-- }}}

-- Val {{{

val :: (V a :<: f) => a -> Mu f
val = review _Val

vals :: (V a :<: f) => Mu f -> [a]
vals = toListOf _Val

_Val :: (V a :<: f) => Prism' (Mu f) a
_Val = _Inj . from _V

-- }}}

-- Pretty {{{

newtype Pretty = Pretty
  { prettyPrec :: Int -> ShowS
  }

instance Show a => Eval Pretty (V a) where
  evalIn (V a) = Pretty $ \d -> showsPrec d a

pretty :: Eval Pretty f => Mu f -> String
pretty = ($ "") . flip prettyPrec 0 . eval

-- }}}

-- Unify {{{

newtype UVar a = UVar { getUVar :: Unknown } deriving (Eq,Show,Functor,Foldable,Traversable)

instance Zip UVar

instance Eval Pretty UVar where
  evalIn (UVar i) = Pretty $ \d ->
    showParen (d > 10)
      $ showString "x_"
      . shows i

type U_ f = UVar :+: f

newtype U f = U
  { unU :: Mu (U_ f)
  }

instance Eq (f (Mu (U_ f))) => Eq (U f) where
  U t == U u = t == u

instance Show (f (Mu (U_ f))) => Show (U f) where
  showsPrec d (U t) = showParen (d > 5)
    $ showString "U "
    . showsPrec 11 t

instance (Functor f, Foldable f) => Partial (U f) where
  unknown   = U . inject . UVar
  isUnknown = fmap getUVar . match . unU
  unknowns  =  map getUVar . nodes . unU
  Substitution s $? U t  = U $ alterNodes go t
    where
    go :: UVar (Mu (U_ f)) -> Mu (U_ f)
    go t@(UVar i) = case H.lookup i s of
      Just (U t') -> t'
      Nothing     -> inject t

newtype UnifyError = UnifyError
  { getError :: String
  } deriving (Eq,Show)

instance Error UnifyError where
  strMsg = UnifyError
  noMsg  = UnifyError "Unification Error"

instance (Functor f, Partial (U f), Zip f, Eval Pretty (U_ f))
  => Unifiable (Either UnifyError) (U f) where
  t =?= u
    | Just (UVar x) <- match $ unU t = x =:= u
    | Just (UVar x) <- match $ unU u = x =:= t
    | otherwise = zipF
      (\a b () -> U a =?= U b)
      (\_ _ -> id)
      (\a b -> UnifyError $ "Couldn't match " ++ pretty (In a) ++ " with " ++ pretty (In b))
      ()
      t' u'
      where
      U (In t') = t
      U (In u') = u

type UnifyM f = UnifyT (U f) (Either UnifyError)

unify :: (Functor f, Foldable f)
  => UnifyM f (U f) -> Either UnifyError (U f)
unify m = runUnify defaultUnifyState m >>= \(a,st) ->
  return $ unifyCurrentSubstitution st $? a

prettyU :: Eval Pretty f => U f -> String
prettyU = pretty . unU

int :: (V Int :<: f) => Int -> U f
int = U . val

bool :: (V Bool :<: f) => Bool -> U f
bool = U . val

var :: Functor f => Unknown -> U f
var = U . inject . UVar

str :: (V String :<: f) => String -> Mu f
str = val

err :: (V UnifyError :<: f) => String -> Mu f
err = val . UnifyError

-- }}}

type ErrorWith e f = (Error (Mu f), e :<: f)

type Err  = V UnifyError

-- Add Mul {{{

data Add r = Add r r deriving (Eq,Show,Functor,Foldable,Traversable)

instance Zip Add where
  zipF scLower scHere fl r e1@(Add a b) e2@(Add c d)
      = scLower a c r
    >>= scLower b d
    >>= return . scHere e1 e2

add :: (Add :<: f) => U f -> U f -> U f
add (U a) (U b) = U $ inject $ Add a b

instance Eval Pretty Add where
  evalIn (Add a b) = Pretty $ \d ->
    showParen (d > prec)
      $ prettyPrec a (prec+1)
      . showString " + "
      . prettyPrec b (prec+1)
    where
    prec = 6

data Mul r = Mul r r deriving (Eq,Show,Functor,Foldable,Traversable)
mul :: (Mul :<: f) => U f -> U f -> U f
mul (U a) (U b) = U $ inject $ Mul a b

instance Zip Mul where
  zipF scLower scHere fl r e1@(Mul a b) e2@(Mul c d)
      = scLower a c r
    >>= scLower b d
    >>= return . scHere e1 e2

instance Eval Pretty Mul where
  evalIn (Mul a b) = Pretty $ \d ->
    showParen (d > prec)
      $ prettyPrec a (prec+1)
      . showString " * "
      . prettyPrec b (prec+1)
    where
    prec = 7

-- }}}

type F    = V Int :+: V Bool :+: Add :+: Mul
type Expr = U F

test1 :: UnifyM F Expr
test1 = do
  e1 <- mul <$> fresh <*> fresh
  return e1

test2 :: UnifyM F Expr
test2 = do
  [x,y] <- replicateM 2 fresh
  let e1 = mul x y
  x =?= int 2
  y =?= int 3
  return e1

test3 :: UnifyM F Expr
test3 = do
  e1 <- mul <$> fresh <*> fresh
  let e2 = add (int 3) (int 4)
  e1 =?= mul e2 e1
  return e1

{-
-- :<=: {{{

subsume :: (f :<=: g) => Mu f -> Mu g
subsume = cata (In . up)

reduce :: (f :<=: g, Traversable g) => Mu g -> Maybe (Mu f)
reduce = cataM (fmap In . down)

class (Functor f, Functor g) => f :<=: g where
  up  :: f a -> g a
  down :: g a -> Maybe (f a)

instance (f :<: g) => f :<=: g where
  up  = inj
  down = prj

instance (f :<=: h, g :<=: h) => (f :+: g) :<=: h where
  up  s = case s of
    InL e -> up e
    InR e -> up e
  down h
    | Just l <- down h = Just $ InL l
    | Just r <- down h = Just $ InR r
    | otherwise          = Nothing

-- }}}
-}

