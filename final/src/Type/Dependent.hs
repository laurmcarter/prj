{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Type.Dependent where

import Control.Applicative
import Control.Arrow ((&&&),first,second)
import Prelude hiding (pi)
import GHC.Exts (Constraint)
import Control.Monad (ap)
import Data.Proxy
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Control.Monad.Trans.Class
import Control.Monad.RWS hiding (Any(..))
import Data.Functor.Identity

data Any (c :: * -> Constraint) = Any { getAny :: forall t. c t => t }

t0 :: Term t => t
t0 = lam $ var 0

-- Term {{{

class Term t where
  ann  :: t -> t -> t
  pi   :: t -> t -> t
  app  :: t -> t -> t
  lam  :: t -> t
  star :: t
  var  :: Int -> t
  par  :: Any Name -> t

instance Term (Any Term) where
  ann e t = Any $ ann (getAny e) (getAny t)
  pi  a b = Any $ pi  (getAny a) (getAny b)
  app f x = Any $ app (getAny f) (getAny x)
  lam e   = Any $ lam (getAny e)
  star    = Any   star
  var i   = Any $ var i
  par x   = Any $ par $ instName x

genTerm  :: Any Term -> Any Term
genTerm x = x

instTerm :: Term t => Any Term -> t
instTerm (Any t) = t

instance (Term t1, Term t2) => Term (t1,t2) where
  ann  (e1,e2) (t1,t2) = (ann e1 t1, ann e2 t2)
  pi   (a1,a2) (b1,b2) = (pi  a1 b1, pi  a2 b2)
  app  (f1,f2) (x1,x2) = (app f1 x1, app f2 x2)
  lam  (e1,e2)         = (lam e1   , lam e2   )
  star                 = (star     , star     )
  var  i               = (var i    , var i    )
  par  x               = (par x    , par x    )

-- }}}

-- Value {{{

class Value v where
  vlam  :: (Any Value -> Any Value) -> v
  vstar ::  v
  vpi   ::  v -> (Any Value -> Any Value) -> v
  vneut ::  Any Neutral -> v

instance Value (Any Value) where
  vlam f  = Any $ vlam $ instValue . f
  vstar   = Any   vstar
  vpi a f = Any $ vpi (getAny a) $ instValue . f
  vneut n = Any $ vneut $ instNeutral n

instance (Value v1, Value v2) => Value (v1,v2) where
  vlam f        = (vlam f   , vlam f  )
  vstar         = (vstar    , vstar   )
  vpi (a1,a2) f = (vpi a1 f , vpi a2 f)
  vneut n       = (vneut n  , vneut n )

genValue  :: Any Value -> Any Value
genValue x = x

instValue :: Value v => Any Value -> v
instValue (Any  v) = v

-- }}}

-- Neutral {{{

class Neutral n where
  npar :: Any Name -> n
  napp :: n -> Any Value -> n

instance Neutral (Any Neutral) where
  npar x   = Any $ npar x
  napp n v = Any $ napp (getAny n) (instValue v)

instance (Neutral n1, Neutral n2) => Neutral (n1,n2) where
  npar x         = (npar x    , npar x)
  napp (n1,n2) v = (napp n1 v , napp n2 v)

genNeutral  :: Any Neutral -> Any Neutral
genNeutral x = x

instNeutral :: Neutral n => Any Neutral -> n
instNeutral (Any n) = n

-- }}}

-- Name {{{

class Name n where
  global   :: String -> n
  bound    :: Int -> n
  unquoted :: Int -> n

instance Name (Any Name) where
  global x   = Any $ global   x
  bound  i   = Any $ bound    i
  unquoted i = Any $ unquoted i

instance (Name n1, Name n2) => Name (n1,n2) where
  global   x = (global x   , global x  )
  bound    i = (bound  i   , bound  i  )
  unquoted i = (unquoted i , unquoted i)

genName  :: Any Name -> Any Name
genName x = x

instName :: Name n => Any Name -> n
instName (Any n) = n

-- }}}

-- Quote {{{

quote0 :: Term t => Quote t -> t
quote0 q = quote q 0

newtype Quote t = Quote
  { quote :: Int -> t
  }

instance Term t => Value (Quote t) where
  vlam  f = Quote $ \i -> lam $ quote (instValue $ f $ vpar $ unquoted i) (i + 1)
  vneut n = Quote $ \i -> quote (instNeutral n) i
  vstar   = Quote $ \i -> star
  vpi a f = Quote $ \i -> pi (quote a i) $ quote (instValue $ f $ vpar $ unquoted i) (i + 1)

instance Term t => Neutral (Quote t) where
  npar x   = Quote $ \i -> varpar (instName x) i
  napp n v = Quote $ \i -> app (quote n i) (quote (instValue v) i)

newtype VarPar t = VarPar
  { varpar :: Int -> t
  }

instance Term t => Name (VarPar t) where
  unquoted k = VarPar $ \i -> var $ i - k - 1
  bound    k = VarPar $ \i -> par $ genName $ bound k
  global   x = VarPar $ \i -> par $ genName $ global x

vpar :: Any Name -> Any Value
vpar = vneut . genNeutral . npar

-- }}}

-- Eval {{{

eval :: Eval -> Any Value
eval e = eval_ e []

newtype Eval = Eval
  { eval_ :: [Any Value] -> Any Value }

instance Term Eval where
  ann e _ = Eval $ \env -> eval_ e env
  app f x = Eval $ \env -> case vapp (eval_ f env) (eval_ x env) of
    Just r -> r
    _      -> error "Malformed exp"
  lam e   = Eval $ \env -> vlam $ \x -> eval_ e (x : env)
  var i   = Eval $ \env -> env !! i
  par x   = Eval $ \_   -> vpar x
  star    = Eval $ \_   -> vstar
  pi a b  = Eval $ \env -> vpi (eval_ a env) $ \x -> eval_ b (x : env)

vapp :: Any Value -> Any Value -> Maybe (Any Value)
vapp v1 v2 = case vapp' $ instValue v1 of
  Just f -> Just $ f v2
  _      -> Nothing

newtype Vapp = Vapp
  { vapp' :: Maybe (Any Value -> Any Value)
  }

instance Value Vapp where
  vneut n = Vapp $ Just $ \v -> vneut $ napp n v
  vlam  f = Vapp $ Just $ \v -> instValue $ f v
  vpi a f = Vapp Nothing
  vstar   = Vapp Nothing

-- }}}

-- Render {{{

data Foo = Foo

newtype RendT i g r m a = Rend
  { unRendT :: RWST i r (Int,g) m a
  } deriving
    ( Functor , Applicative , Monad
    , MonadTrans
    , MonadReader i
    , MonadWriter r
    , MonadState (Int,g)
    )

type Rend i g r = RendT i g r Identity

symbol :: Monoid r => r -> Rend i g r ()
symbol = tell

token :: r -> Rend i g [r] ()
token = tell . (:[])

curPrec :: Monoid r => Rend i g r Int
curPrec = gets fst

curGen :: Monoid r => Rend i g r g
curGen = gets snd

onPrec :: Monoid r => (Int -> Int) -> Rend i g r ()
onPrec = modify . first

onGen :: Monoid r => (g -> g) -> Rend i g r ()
onGen = modify . second

newPrec :: Monoid r => Int -> Rend i g r ()
newPrec = onPrec . const

newGen :: Monoid r => g -> Rend i g r ()
newGen = onGen . const

parens :: Int -> Rend String i g a -> Rend String i g a
parens p m = do
  c <- curPrec
  newPrec 
  if c > p
    then do
      token '('
      m
      token ')'
    else m

{-
runRend :: Rend r i g a -> i -> Int -> g -> (a,(Int,g),r)
runRend m i p g = runRWS (unRendT m) i (p,g)
-}

{-
execRend :: Rend r i g a -> i -> Int -> g -> r
execRend m i p g = f
  where
  (_,_,f) = runRend m i p g
-}

{-
class Render r i g a | a -> i g where
  rendersPrec :: a -> Rend r i g ()
  render :: i -> g -> a -> r
  render i g a = execRend m i 0 g mempty
    where
    m :: Rend r i g ()
    m = rendersPrec a
-}

newtype LiftedShow a = LiftShow
  { lowerShow :: a
  }

{-
instance Show a => Render String i g (LiftedShow a) where
  rendersPrec a = 
-}

-- }}}

-- Supply {{{

class Supply s where
  type Sup s
  newSupply :: s
  supply1   :: s -> (s,Sup s)

class (Supply s, Monad m) => MonadSupply s m | m -> s where
  supply :: m (Sup s)

newtype SupplyT s m a = SupplyT
  { runSupplyT :: s -> m (s,a)
  }

instance Functor m => Functor (SupplyT s m) where
  fmap f (SupplyT m) = SupplyT $ fmap (fmap f) . m

instance (Functor m, Monad m) => Applicative (SupplyT s m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (SupplyT s m) where
  return a = SupplyT $ \s -> return (s,a)
  m >>= f  = SupplyT $ \s0 -> do
    (s1,a) <- runSupplyT m s0
    runSupplyT (f a) s1

instance (Monad m, Supply s) => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ return . supply1

instance Supply Int where
  type Sup Int = Int
  newSupply = 0
  supply1 s = (s + 1,s)

evalSupplyT :: (Monad m, Supply s) => SupplyT s m a -> m (s,a)
evalSupplyT = runSupplyT $$ newSupply

($$) :: (a -> b -> c) -> b -> a -> c
($$) = flip
infixr 1 $$

-- }}}

{-
type Result = Either String

throw :: String -> Result a
throw = Left

newtype TypeI = TypeI
  { infType :: Int -> [(Any Name,Any Value)] -> Result (Any Value)
  }

newtype TypeC = TypeC
  { chkType :: Int -> [(Any Name,Any Value)] -> Any Value -> Result ()
  }

instance Term TypeI where
  ann e t = TypeI $ \i cxt -> do
    t1 <- infType e i cxt
    case instValue t1 of
-}
  
{-
data f :< t
  = Capture { capture :: f (f :< t) }
  | Other   { other   :: t }
infixr 4 :<

pTerm :: Proxy Term
pTerm = Proxy

pValue :: Proxy Value
pValue = Proxy

pNeutral :: Proxy Neutral
pNeutral = Proxy

pName :: Proxy Name
pName = Proxy

instance (Term t, Value v, Neutral n) => Term (TermI t v n) where
  ann  e t = TI $ Right $ Ann _ _
  pi   a b = undefined
  app  f x = undefined
  lam  e   = undefined
  star     = undefined
  var  i   = undefined
  par  x   = undefined

newtype Mu f = Mu { mu :: f (Mu f) }

newtype Nu f = Nu { nu :: f (Nu f) }

cata :: Functor f => (f r -> r) -> Mu f -> r
cata f = f . fmap (cata f) . mu

ana :: Functor f => (r -> f r) -> r -> Nu f
ana f = Nu . fmap (ana f) . f

para :: Functor f => (f (Mu f,r) -> r) -> Mu f -> r
para f = f . fmap (id &&& para f) . mu

apo :: Functor f => (r -> f (Either (Nu f) r)) -> r -> Nu f
apo f = Nu . fmap (either id $ apo f) . f

newtype TermI t v n = TI
  { termI :: Either t (Term_I (TermC t v n) (TermI t v n))
  }

newtype TermC t v n = TC
  { termC :: Either t (Term_C (TermI t v n) (TermC t v n))
  }

newtype Val t v n = V
  { val :: Either v (Value_ (Neut t v n) (Val t v n))
  }

newtype Neut t v n = N
  { neut :: Either n (Neutral_ (Val t v n) (Neut t v n))
  }

data Term_I c i
  = Ann c c
  | Star
  | Pi c c
  | Var Int
  | Par Name_
  | i :@: c

data Term_C i c
  = Inf i
  | Lam c

data Value_ n v
  = VLam (v -> v)
  | VStar
  | VPi v (v -> v)
  | VNeutral n

data Neutral_ v n
  = NPar Name_
  | NApp n v

data Name_
  = Global   String
  | Bound    Int
  | Unquoted Int
-}

{-
data TermCase t
  = Ann (TermCase t) (TermCase t)
  | Star
  | Pi (TermCase t) (TermCase t)
  | Var Int
  | Par (NameCase n)
  | App (TermCase t) (TermCase t)
  | Lam (TermCase t)
  | TLift t

instance Term t => Term (TermCase t) where
  -- type TName (TermCase t) = NameCase (TName t)
  ann  = Ann
  star = Star
  pi   = Pi
  var  = Var
  par x  = Par _
  app  = App
  lam  = Lam

term :: Term t => TermCase t -> t
term = \case
  Ann e t -> ann (term e) (term t)
  Star    -> star
  Pi a b  -> pi (term a) (term b)
  Var b   -> var b
  Par f   -> par (name f)
  App f x -> app (term f) (term x)
  Lam e   -> lam (term e)
  TLift t -> t
-}

{-
data ValueCase v
  = VLam (ValueCase v -> ValueCase v)
  | VStar
  | VPi (ValueCase v) (ValueCase v -> ValueCase v)
  | VNeutral (NeutralCase (VNeut v))
  | VLift v

instance (Value v, Neutral (VNeut v)) => Value (ValueCase v) where
  type VNeut (ValueCase v) = NeutralCase (VNeut v)
  vlam  = VLam
  vstar = VStar
  vpi   = VPi
  vneut = VNeutral

value :: (Value v, Neutral (VNeut v), Name (NName (VNeut v)))
  => ValueCase v -> v
value = \case
  VLam f     -> vlam $ value . f . VLift
  VStar      -> vstar
  VPi a f    -> vpi (value a) $ value . f . VLift
  VNeutral n -> vneut $ neutral n
  VLift v    -> v
-}

{-
data NeutralCase n
  = NPar (NName (NeutralCase n))
  | NApp (NeutralCase n) (ValueCase (NVal n))
  | NLift n

instance (Neutral n, Value (NVal n), Name (NName n))
  => Neutral (NeutralCase n) where
  type NVal  (NeutralCase n) = ValueCase (NVal n)
  type NName (NeutralCase n) = NameCase (NName n)
  npar = NPar
  napp = NApp

neutral :: (Neutral n, Value (NVal n), Name (NName n)) => NeutralCase n -> n
neutral = \case
  NPar nm  -> npar (name nm)
  NApp f x -> napp (neutral f) (value x)
  NLift n  -> n
-}

{-
data NameCase n
  = Global String
  | Bound Int
  | Unquoted Int
  | NmLift n

instance Name n => Name (NameCase n) where
  global   = Global
  bound    = Bound
  unquoted = Unquoted

name :: Name n => NameCase n -> n
name = \case
  Global n   -> global n
  Bound  i   -> bound i
  Unquoted i -> unquoted i
  NmLift n   -> n
-}

