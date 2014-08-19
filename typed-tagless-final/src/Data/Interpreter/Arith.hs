{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Interpreter.Arith where

import Control.Applicative
import Control.Arrow ((&&&),(***))
import Control.Monad ((>=>))
import Control.Monad.Trans.Cont
import Control.Monad.Error.Class
import Data.Functor.Constant
import Data.Functor.Product

import GHC.Exts (Constraint)
import Data.Proxy

import Debug.Trace

class Lit r where
  lit :: Int -> r Int

class Neg r where
  neg :: r Int -> r Int

class Add r where
  add :: r Int -> r Int -> r Int

class Mul r where
  mul :: r Int -> r Int -> r Int

class LC r where
  lam :: (r a -> r b) -> r (a -> b)
  app :: r (a -> b) -> r a -> r b

type Arith = Lit :*: Neg :*: Add :*: Mul

class    (f a, g a) => (f :*: g) a
instance (f a, g a) => (f :*: g) a
infixr 4 :*:

arith :: Proxy Arith
arith = Proxy

-- I {{{

type C = Constant
pattern C a = Constant a
getC :: C a b -> a
getC = getConstant

type I = C Int

_const :: (a -> b) -> C a c -> C b d
_const f a = C $ f $ getC a

_const2 :: (a -> b -> c) -> C a d -> C b e -> C c f
_const2 f a b = C $ f (getC a) (getC b)

-- }}}

-- Render {{{

newtype R a = R { unR :: a } deriving (Eq,Show)

instance Functor R where
  fmap f (R a) = R $ f a

instance Applicative R where
  pure = R
  R f <*> R x = R $ f x

instance Monad R where
  return = R
  R m >>= f = f m

-- }}}

-- PushNeg {{{

newtype PushNeg r a = PushNeg
  { pushNeg_ :: Bool -> r a
  }

pushNeg :: Arith r => PushNeg r a -> r a
pushNeg p = pushNeg_ p True

cond :: a -> a -> Bool -> a
cond t f b = if b then t else f

instance Lit r => Lit (PushNeg r) where
  lit n = PushNeg $ lit . cond n (negate n)

instance Neg r => Neg (PushNeg r) where
  neg a = PushNeg $ pushNeg_ a . not

instance Add r => Add (PushNeg r) where
  add a b = PushNeg $ add <$> pushNeg_ a <*> pushNeg_ b

instance Mul r => Mul (PushNeg r) where
  mul a b = PushNeg $ mul <$> pure (pushNeg_ a True) <*> pushNeg_ b

-- }}}

-- DistMul {{{

distMul :: Arith r => DistMul r a -> r a
distMul d = distMul_ d Nothing

newtype DistMul r a = DistMul
  { distMul_ :: Maybe (r a) -> r a
  }

instance (Mul r, Lit r) => Lit (DistMul r) where
  lit n = DistMul $ maybe (lit n) (mul (lit n))

instance Neg r => Neg (DistMul r) where
  neg a = DistMul $ neg . distMul_ a

instance Add r => Add (DistMul r) where
  add a b = DistMul $ add <$> distMul_ a <*> distMul_ b

instance Mul r => Mul (DistMul r) where
  mul a b = DistMul $ distMul_ b . Just . distMul_ a

-- }}}

-- Eval {{{

eval :: I a -> Int
eval = getC

instance Lit I where
  lit = C

instance Neg I where
  neg = _const negate

instance Add I where
  add = _const2 (+)

instance Mul I where
  mul = _const2 (*)



instance Lit R where
  lit = pure

instance Neg R where
  neg = fmap negate

instance Add R where
  add = liftA2 (+)

instance Mul R where
  mul = liftA2 (*)

-- }}}

-- Render {{{

newtype S a = S
  { showS :: Int -> String -> String
  }

render :: S a -> String
render s = showS s 0 ""

instance Lit S where
  lit n = S $ \d -> showsPrec d n

instance Neg S where
  neg a = S $ \d ->
    showParen (d > 10)
    $ showString "- "
    . showS a 11

instance Add S where
  add a b = S $ \d ->
    showParen (d > 10)
    $ showS a 11
    . showString " + "
    . showS b 11

instance Mul S where
  mul a b = S $ \d ->
    showParen (d > 10)
    $ showS a 11
    . showString " * "
    . showS b 11

-- }}}

-- Duplicate {{{

duplicate :: Product f g a -> (f a,g a)
duplicate (Pair f g) = (f,g)

instance (Lit a, Lit b) => Lit (Product a b) where
  lit n = Pair (lit n) (lit n)

instance (Neg a, Neg b) => Neg (Product a b) where
  neg (Pair a b) = Pair (neg a) (neg b)

instance (Add a, Add b) => Add (Product a b) where
  add (Pair a b) (Pair c d) = Pair (add a c) (add b d)

instance (Mul a, Mul b) => Mul (Product a b) where
  mul (Pair a b) (Pair c d) = Pair (mul a c) (mul b d)

-- }}}

-- {De}Serialize {{{

data Tree
  = Leaf String
  | Node String [Tree]
  deriving (Eq,Show)

type T = C Tree

toTree :: T a -> T a
toTree = id

instance Lit T where
  lit n = C $ Node "Lit" [Leaf $ show n]

instance Neg T where
  neg a = C $ Node "Neg" [getC a]

instance Add T where
  add a b = C $ Node "Add" [getC a, getC b]

instance Mul T where
  mul a b = C $ Node "Mul" [getC a, getC b]

{-
fromTree :: (FromTree c, c r) => Proxy c -> T -> Either Err r
fromTree p = go
  where
  go   = fromTree' p go base
  base t = Left $ "No parse ("++msg p++"): "++show t
-}

match :: (Tree -> Maybe (Either Err r)) -> (Tree -> Either Err r) -> Tree -> Either Err r
match f fl t = case f t of
  Just e -> e
  _      -> fl t

data Some r = forall t. Some (r t)

class FromTree c where
  msg :: Proxy c -> String
  fromTree' :: c r => Proxy c -> (Tree -> Either Err (Some r))
    -> (Tree -> Either Err (Some r)) -> (Tree -> Either Err (Some r))

instance (FromTree c1, FromTree c2) => FromTree (c1 :*: c2) where
  msg _ = msg (Proxy :: Proxy c1) ++ " " ++ msg (Proxy :: Proxy c2)
  fromTree' _ go =
      fromTree' (Proxy :: Proxy c1) go
    . fromTree' (Proxy :: Proxy c2) go

instance FromTree Lit where
  msg _ = "Lit"
  fromTree' _ go = match $ \case
    Node "Lit" [Leaf n] -> Just $ Some . lit <$> safeRead n
    _                   -> Nothing

{-
instance FromTree Neg where
  msg _ = "Neg"
  fromTree' _ go = match $ \case
    Node "Neg" [a] -> Just $ Some . neg <$> go a
    _              -> Nothing
-}

{-
instance FromTree Add where
  msg _ = "Add"
  fromTree' _ go = match $ \case
    Node "Add" [a,b] -> Just $ add <$> go a <*> go b
    _                -> Nothing

instance FromTree Mul where
  msg _ = "Mul"
  fromTree' _ go = match $ \case
    Node "Mul" [a,b] -> Just $ mul <$> go a <*> go b
    _                -> Nothing
-}

-- Util

safeRead :: Read a => String -> Either Err a
safeRead s = case reads s of
  [(x,"")] -> return x
  _        -> Left $ "Read error: " ++ s

-- }}}

type Err = String


checkConsume :: (a -> IO ()) -> Either Err a -> IO ()
checkConsume f = \case
  Left  e -> putStrLn $ "Error: " ++ e
  Right a -> f a

dupConsume :: Show r => (f a -> r) -> Product f g a -> IO (g a)
dupConsume ev x = print (ev x1) >> return x2
  where
  (x1,x2) = duplicate x

t1 :: Arith r => r Int
t1 = add (lit 8) (neg (add (lit 1) (lit 2)))

t2 :: Arith r => r Int
t2 = mul t1 (add (neg (mul (lit 3) (lit 4))) (lit 5))

