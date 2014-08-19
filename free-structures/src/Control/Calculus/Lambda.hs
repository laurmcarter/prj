{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Control.Calculus.Lambda where

import Control.Applicative
import Control.Arrow (first,second)

type f :->  g = forall a. f a  -> g a
type r :->: s = forall a. r a :-> s a


-- LamI {{{

data LamI r env a where
  Var  :: VarI r  env    a
       -> LamI r  env    a
  Lam  :: LamI r (env,a) b
       -> LamI r  env   (a -> b)
  App  :: LamI r  env   (a -> b)
       -> LamI r  env    a
       -> LamI r  env    b
  Int  :: Int
       -> LamI r  env    Int
  Add  :: LamI r  env    Int
       -> LamI r  env    Int
       -> LamI r  env    Int
  Neg  :: LamI r  env    Int
       -> LamI r  env    Int
  LLft ::      r  env    a
       -> LamI r  env    a

initLam :: LamI r env a -> LamI r env a
initLam = id

transLam :: (r :->: s) -> LamI r env a -> LamI s env a
transLam f t = case t of
  Var  x     -> Var  $ transVar f x
  Lam  e     -> Lam  $ transLam f e
  App  tf tx -> App  ( transLam f tf )
                     ( transLam f tx )
  Int  i     -> Int    i
  Add  x  y  -> Add  ( transLam f x  )
                     ( transLam f y  )
  Neg  x     -> Neg  ( transLam f x  )
  LLft r     -> LLft $ f r

liftLam :: r env a -> LamI r env a
liftLam = LLft

-- }}}

-- Free Lam {{{

class FreeLambda r where
  var :: VarI r  env    a
      ->      r  env    a
  lam :: LamI r (env,a) b
      ->      r  env   (a -> b)
  app :: LamI r  env   (a -> b)
      -> LamI r  env    a
      ->      r  env    b
  int :: Int
      ->      r  env    Int
  add :: LamI r  env    Int
      -> LamI r  env    Int
      ->      r  env    Int
  neg :: LamI r  env    Int
      ->      r  env    Int

llam :: FreeLambda r => LamI (LamI r) env a -> LamI r env a
llam = transLam fl

lvar :: FreeVar r => LamI (VarI r) env a -> LamI r env a
lvar = transLam fv

-- | finalize one layer
fl :: FreeLambda r => LamI r env a -> r env a
fl t = case t of
  Var  x    -> var x
  Lam  e    -> lam e
  App  f x  -> app f x
  Int  i    -> int i
  Add  x y  -> add x y
  Neg  x    -> neg x
  LLft x    -> x

instance FreeLambda r => FreeLambda (LamI r) where
  var = Var .  vlam
  lam = Lam .  fl
  app = App .* fl
  int = Int
  add = Add .* fl
  neg = Neg .  fl

-- }}}

-- Lam {{{

class Lambda r where
  var_ :: VarI r  env a
       ->      r  env a
  lam_ ::      r (env,a) b
       ->      r  env   (a -> b)
  app_ ::      r  env   (a -> b)
       ->      r  env    a
       ->      r  env    b
  int_ :: Int
       ->      r  env    Int
  add_ ::      r  env    Int
       ->      r  env    Int
       ->      r  env    Int
  neg_ ::      r  env    Int
       ->      r  env    Int

instance Lambda r => Lambda (LamI r) where
  var_ = Var . varLowerLam
  lam_ = Lam
  app_ = App
  int_ = Int
  add_ = Add
  neg_ = Neg

-- | finalize all layers
finalLam :: Lambda r => LamI r env a -> r env a
finalLam t = case t of
  Var  x    -> var_ x
  Lam  e    -> lam_ $ finalLam e
  App  f x  -> app_ ( finalLam f )
                    ( finalLam x )
  Int  i    -> int_ i
  Add  x y  -> add_ ( finalLam x )
                    ( finalLam y )
  Neg  x    -> neg_ ( finalLam x )
  LLft x    -> x

lamLowerLam :: Lambda r => LamI (LamI r) env a -> LamI r env a
lamLowerLam = transLam finalLam

lamLowerVar :: Var r => LamI (VarI r) env a -> LamI r env a
lamLowerVar = transLam finalVar

-- }}}


-- VarI {{{

data VarI r env a where
  Z    :: VarI r (env,a) a
  S    :: VarI r  env    a
       -> VarI r (env,b) a
  VLft ::      r  env    a
       -> VarI r  env    a

initVar :: VarI r env a -> VarI r env a
initVar = id

transVar :: (r :->: s) -> VarI r env a -> VarI s env a
transVar f t = case t of
  Z      -> Z
  S x    -> S $ transVar f x
  VLft r -> VLft $ f r

-- }}}

-- Free Var {{{

class FreeVar r where
  z ::      r (env,a) a
  s :: VarI r  env    a
    ->      r (env,b) a

instance FreeVar (VarI r) where
  z   = Z
  s x = S $ fv x

fv :: FreeVar r => VarI r env a -> r env a
fv v = case v of
  Z      -> z
  S x    -> s x
  VLft x -> x

vlam :: FreeLambda r => VarI (LamI r) env a -> VarI r env a
vlam = transVar fl

vvar :: FreeVar r => VarI (VarI r) env a -> VarI r env a
vvar = transVar fv

-- }}}

-- Var {{{

class Var r where
  z_ :: r (env,a) a
  s_ :: r  env    a
     -> r (env,b) a

instance Var (VarI r) where
  z_ = Z
  s_ = S

finalVar :: Var r => VarI r env a -> r env a
finalVar v = case v of
  Z      -> z_
  S x    -> s_ $ finalVar x
  VLft x -> x

varLowerLam :: Lambda r => VarI (LamI r) env a -> VarI r env a
varLowerLam = transVar finalLam

varLowerVar :: Var r => VarI (VarI r) env a -> VarI r env a
varLowerVar = transVar finalVar

-- }}}


-- ShowF {{{

type ShowG = (String,Int) -> (String,Int)
sg :: ShowS -> ShowG
sg = first

showParenG :: Bool -> ShowG -> ShowG
showParenG b p = if b
  then sg (showChar '(') . p . sg (showChar ')')
  else p

bind :: (Int -> ShowG) -> ShowG
bind f (inp,gen) = f gen (inp,gen + 1)

incr :: ShowG -> ShowG
incr f = second pred . f . second succ

decr :: ShowG -> ShowG
decr f = second succ . f . second pred

reset :: ShowG -> ShowG
reset f (inp,gen) = (inp',gen)
  where
  (inp',_) = f (inp,0)

newtype ShowF env a = ShowF
  { showsPrecF :: Int -> ShowG
  }

showDefn :: String -> ShowF env a
showDefn = ShowF . const . sg . showString

showsPrecDefn :: (Int -> ShowG) -> ShowF env a
showsPrecDefn = ShowF

current :: (Int -> ShowG) -> ShowG
current f p@(inp,gen) = f gen p

instance FreeVar ShowF where
  z   = showsPrecDefn $ \_ ->
      current $ \i ->
      sg $ showString
    $ if (i < 0)
      then "fv." ++ show (1 - i)
      else "x."  ++ show i
  s x = showsPrecDefn $ \d ->
      decr $ showsPrecF (fv x) d

instance FreeLambda ShowF where
  var x   = showsPrecDefn $ \d ->
      decr
    $ showsPrecF (fv x) d
  lam t   = showsPrecDefn
    $ \d -> bind $ \i ->
      showParenG (d > 10)
    $ sg (showString $ "\\x." ++ show i ++ " ")
    . case t of
        Lam t' -> sg tail
                  -- ^ drop the \ from the next lambda
                . showsPrecF (fl t) 0
        _      -> sg (showString "-> ")
                  -- ^ no more bindings, can add the arrow
                . showsPrecF (fl t) 0
  app f x = showsPrecDefn $ \d ->
      showParenG (d > 10)
    $ showsPrecF (fl f) 11
    . sg (showChar ' ')
    . showsPrecF (fl x) 11
  int i   = showDefn $ show i
  add x y = showsPrecDefn $ \d ->
      showParenG (d > 6)
    $ showsPrecF (fl x) 7
    . sg (showString " + ")
    . showsPrecF (fl y) 7
  neg x   = showsPrecDefn $ \d ->
      showParenG (d > 10)
    $ sg (showString "negate ")
    . showsPrecF (fl x) 11

instance Show (ShowF env a) where
  showsPrec d (ShowF f) = fst . f d . flip (,) 0

showF :: ShowF env a -> ShowF env a
showF = id

-- }}}


-- Eval {{{

newtype Eval env a = Eval
  { eval :: env -> a
  }

instance FreeVar Eval where
  z   = Eval   snd
  s x = Eval $ eval (fv x) . fst

instance FreeLambda Eval where
  var x   = Eval $ eval (fv x)
  lam t   = Eval $ \env a -> eval (fl t) (env,a)
  app f x = Eval $ eval (fl f) <*> eval (fl x)
  int i   = Eval $ pure i
  add x y = Eval $ (+) <$> eval (fl x) <*> eval (fl y)
  neg x   = Eval $ negate <$> eval (fl x)

-- | Force evaluation to be of a closed, grounded term
evalDefn :: Eval () a -> a
evalDefn t = eval t ()

-- }}}

ifNeg :: a -> a -> Bool -> a
ifNeg t f b = if b then t else f

newtype PushNeg r env a = PushNeg
  { pushNeg :: Bool -> r env a
  }

pushNegations :: PushNeg r env a -> r env a
pushNegations = pushNeg $$ False

{-

transVar and transLam maintain their free structure,
while applying a transformation to the remaining, uncaptured
structure in their 'VLft' and 'LLft' constructions.
These transformations are applied recursively.

-}

instance FreeLambda r => FreeLambda (PushNeg r) where
  var x   = PushNeg $ \isNeg ->
    var $ transVar (pushNeg $$ isNeg) x
  ----
  lam t   = PushNeg $ \isNeg ->
    lam $ undefined $ pushNeg (fl t) isNeg
  ----
  app f x = PushNeg $ \isNeg ->
    app ( undefined $ pushNeg (fl f) isNeg )
        ( undefined $ pushNeg (fl x) isNeg )
  ----
  int i   = PushNeg $
    ifNeg ( int $ negate i )
          ( int          i )
  ----
  add x y = PushNeg $ \isNeg ->
    add ( undefined $ pushNeg (fl x) isNeg )
        ( undefined $ pushNeg (fl y) isNeg )
  ----
  neg x   = PushNeg $ pushNeg (fl x) . not


-- Examples {{{

-- | univeral quantification over env
--   means that the terms are closed,
--   but they can be used in any context
type Defn a = forall r env. FreeLambda r => r env a

t0 :: Defn Int
t0 = int 3

t1 :: Defn Int
t1 = t0 `add` t0

t2 :: Defn (Int -> Int -> Int)
t2 = lam $ lam $ add (var z) (var (s z))

-- term isn't closed, therefore can't be typed as a Defn
-- t3 :: Defn a
t3 :: FreeLambda r => r ((env,a),b) a
t3 = var $ s z

t4 :: Defn ((a -> b) -> a -> b)
t4 = lam $ lam $ app (var $ s z) (var z)

-- fix combinator
t5 :: Defn ((a -> a) -> a)
t5 = lam $ app    (var z)
         $ app t5 (var z)

t6 :: Defn (Int -> Int)
t6 = lam $ neg $ add (var z) (var z)

-- }}}


-- Util {{{

($$) :: (a -> b -> c) -> b -> a -> c
($$) = flip
infixr 1 $$

(.*) :: (g a -> g b -> r) -> (f :-> g) -> f a -> f b -> r
(f .* g) x y = f (g x) (g y)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- }}}

