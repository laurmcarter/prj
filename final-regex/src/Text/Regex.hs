{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Text.Regex where

import Data.Constraint.Recover

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.List (nub,stripPrefix,intersperse)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Prelude.Extras
import Data.Constraint
import Data.Type.Equality
import Data.Functor.Identity
import Debug.Trace

type a * b = (a,b)
type a + b = Either a b

data R r a where
  Int  :: R r Int
  Char :: R r Char
  (:&) :: R r a -> R r b -> R r (a,b)
  Lift :: r a -> R r a
infixr 4 :&

newtype Cnst r a = Cnst
  { getCnst :: r
  } deriving (Eq,Show)

instance Show r => Show1 (Cnst r) where
  showsPrec1 d (Cnst r) = showParen (d > 10)
    $ showString "Cnst "
    . showsPrec 11 r

type Dummy = Cnst ()

f0 :: R Dummy (Int,(Char,Int))
f0 = Int :& Char :& Int

instance (Show a, Show1 (R r)) => Show (R r a) where
  showsPrec = showsPrec1

instance Show1 r => Show1 (R r) where
  showsPrec1 d (r :: R r a) = case r of
    Int    -> showString "Int"
    Char   -> showString "Char"
    (p :: R r b) :& (q :: R r c) -> showParen (d > 4)
      $ showsPrec1 5 p
      . showString " :& "
      . showsPrec1 5 q
      \\ (recover :: Show a :- (Show b, Show c))
    Lift r -> showParen (d > 10)
      $ showString "Lift "
      . showsPrec1 11 r

class Regex r where
  int  :: r Int
  char :: r Char
  str  :: String -> r ()
  (&*) :: r a -> r b -> r (a,b)
  (|*) :: r a -> r b -> r (Either a b)
  star :: r a -> r [a]
  plus :: r a -> r (a,[a])
infixr 4 &*
infixr 3 |*

fmt0 :: Regex r => r (Int,(Char,Int))
fmt0 = int &* char &* int

fmt1 :: Regex r => r [Int]
fmt1 = star int

fmt2 :: Regex r => r (Int,())
fmt2 = int &* str "foo"

fmt3 :: Regex r => r [Either Int Char]
fmt3 = star $ int |* char

-- Match {{{

newtype Match a = Match
  { unMatch :: Rd a
  } deriving
  ( Functor
  , Applicative
  , Alternative
  , Monad
  , MonadPlus
  , MonadState String
  )

runMatch :: Match a -> String -> [(a,String)]
runMatch = runStateT . unMatch

match :: Match a -> String -> [a]
match = evalStateT . unMatch

instance Regex Match where
  int    = satRead1 isDigit
  char   = get1
  str s  = do
    inp <- get
    case stripPrefix s inp of
      Just inp' -> put inp'
      _         -> mzero
  (&*)   = liftM2 (,)
  (|*)   = liftMP2 Left Right
  star m = mplus (return []) ((:) <$> m <*> star m)
  plus m = (,) <$> m <*> star m

-- }}}

newtype StT m s i j a = StT
  { unStT :: s i -> m (s j, a)
  }

newtype St s i j a = St
  { unSt :: (s,i) -> ((s,j),a)
  }

instance Functor (St s i j) where
  fmap f (St m) = St $ fmap f . m

instance (i ~ j) => Applicative (St s i j) where
  pure  = return
  (<*>) = ap

instance (i ~ j) => Monad (St s i j) where
  return  = ireturn
  (>>=) = (>>>=)

class IxApplicative (m :: x -> x -> * -> *) where
  ipure  :: a -> m i i a
  (<**>) :: m i j (a -> b) -> m j k a -> m i k b

class IxMonad (m :: x -> x -> * -> *) where
  ireturn :: a -> m i i a
  (>>>=)  :: m i j a -> (a -> m j k b) -> m i k b

instance IxApplicative (St s) where
  ipure  = ireturn
  (<**>) = iap

instance IxMonad (St s) where
  ireturn a = St $ \si -> (si,a)
  m >>>= f  = St $ \si ->
    let (sj,a) = unSt m si
    in unSt (f a) sj

iap :: IxMonad m => m i j (a -> b) -> m j k a -> m i k b
iap mf mx = mf >>>= \f -> mx >>>= \x -> ireturn $ f x

{-
newtype Print a = Print
  { unPrint :: a -> Sh'
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader Int
  , MonadWriter String
  )
-}

{-
instance Regex Print where
  int
-}

-- MonadShow {{{

type MonadShow m =
  ( Applicative m
  , MonadReader Int m
  , MonadWriter String m
  )

type Sh  = ReaderT Int (Writer String)
type Sh' = Sh ()

shChar :: Char -> Sh'
shChar c = tell [c]

shStr  :: String -> Sh'
shStr = tell

shs :: Show a => a -> Sh'
shs a = do
  d <- ask
  shStr $ showsPrec d a ""

withPrec :: Int -> Sh a -> Sh a
withPrec = local . const

parenPrec :: Int -> Sh a -> Sh a
parenPrec i m = do
  d <- ask
  withPrec (i+1) $ if (d > i)
    then shChar '(' *> m <* shChar ')'
    else m

words :: [Sh'] -> Sh'
words = sequence_ . intersperse (shChar ' ')

-- }}}

-- MonadRead {{{

type MonadRead m =
  ( Applicative m
  , MonadPlus m
  , MonadState String m
  )

type Rd  = StateT String []
type Rd' = Rd ()

satisfies1 :: MonadRead m
  => (String -> a) -> (Char -> Bool) -> m a
satisfies1 f pr = f <$> go1 go
  where
  go1 m = do
    c <- get1
    guard $ pr c
    (c:) <$> m
  go = return "" `mplus` go1 go

satisfy1 :: MonadRead m => (Char -> Bool) -> m String
satisfy1 = satisfies1 id

satRead1 :: (MonadRead m, Read a) => (Char -> Bool) -> m a
satRead1 = satisfies1 read

satisfies :: MonadRead m
  => (String -> a) -> (Char -> Bool) -> m a
satisfies f pr = f <$> go
  where
  go = return "" `mplus` do
    c <- get1
    guard $ pr c
    (c:) <$> go

satisfy :: MonadRead m => (Char -> Bool) -> m String
satisfy = satisfies id

satRead :: (MonadRead m, Read a) => (Char -> Bool) -> m a
satRead = satisfies read

get1 :: MonadRead m => m Char
get1 = do
  inp <- get
  case inp of
    [] -> mzero
    c:inp' -> put inp' >> return c

atEnd :: MonadRead m => m a -> m a
atEnd m = do
  a <- m
  inp <- get
  guard $ null inp
  return a

liftMP2 :: MonadPlus m => (a -> c) -> (b -> c) -> m a -> m b -> m c
liftMP2 f g a b = mplus
  (liftM f a)
  (liftM g b)

-- }}}

