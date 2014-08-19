{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Interpreter.ParsePretty where

import Prelude hiding (id,(.))
import Control.Lens hiding (uncons)
import qualified Control.Lens as L
import Control.Category
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Monoid (Monoid(..),(<>))
import GHC.Exts (Constraint)

-- Greedy {{{

newtype Greedy m a = Greedy
  { greedy :: m a
  } deriving (Eq,Show)

instance Functor m => Functor (Greedy m) where
  fmap f = Greedy . fmap f . greedy

instance Applicative m => Applicative (Greedy m) where
  pure    = Greedy . pure
  f <*> x = Greedy $ greedy f <*> greedy x

instance Monad m => Monad (Greedy m) where
  return  = Greedy . return
  m >>= f = Greedy $ greedy m >>= greedy . f

-- prefer b over a
instance Alternative m => Alternative (Greedy m) where
  empty   = Greedy empty
  a <|> b = Greedy $ greedy b <|> greedy a

-- prefer b over a
instance MonadPlus m => MonadPlus (Greedy m) where
  mzero     = Greedy mzero
  mplus a b = Greedy $ greedy b `mplus` greedy a

-- }}}

-- NonGreedy {{{

newtype NonGreedy m a = NonGreedy
  { nongreedy :: m a
  } deriving (Eq,Show)

instance Functor m => Functor (NonGreedy m) where
  fmap f = NonGreedy . fmap f . nongreedy

instance Applicative m => Applicative (NonGreedy m) where
  pure    = NonGreedy . pure
  f <*> x = NonGreedy $ nongreedy f <*> nongreedy x

instance Monad m => Monad (NonGreedy m) where
  return  = NonGreedy . return
  m >>= f = NonGreedy $ nongreedy m >>= nongreedy . f

-- prefer a over b
instance Alternative m => Alternative (NonGreedy m) where
  empty   = NonGreedy empty
  a <|> b = NonGreedy $ nongreedy a <|> nongreedy b

-- prefer a over b
instance MonadPlus m => MonadPlus (NonGreedy m) where
  mzero     = NonGreedy mzero
  mplus a b = NonGreedy $ nongreedy a `mplus` nongreedy b

-- }}}

type All    a = [a]
type Single a = Maybe a

class Format (r :: * -> * -> * -> *) where
  type Stream  r s t :: Constraint
  type Stream0 r s   :: Constraint
  tok       :: Stream  r s t => t           -> r s a         a
  anyTok    :: Stream  r s t =>                r s a ( t  -> a)
  symbol    :: Stream  r s t => [t]         -> r s a         a
  anySymbol :: Stream  r s t =>                r s a ([t] -> a)
  satisfy   :: Stream  r s t => (t -> Bool) -> r s a ( t  -> a)
  satisfies :: Stream  r s t => (t -> Bool) -> r s a ([t] -> a)
  oneOf     :: Stream  r s t => [t]         -> r s a ( t  -> a)
  noneOf    :: Stream  r s t => [t]         -> r s a ( t  -> a)
  end       :: Stream0 r s   =>                r s a         a

-- PP {{{

pPrint :: StreamOf s t => PP s s b -> b
pPrint p = pretty p id

newtype PP s a b = PP
  { pretty :: (s -> a) -> b
  }

instance Format PP where
  type Stream PP s t = StreamOf s t
  type Stream0 PP s  = Monoid s
  tok t     = PP $ \k -> k $ single t
  anyTok = PP $ \k t -> k $ single t
  symbol ts = PP $ \k -> k $ fromList ts
  anySymbol = PP $ \k -> k . fromList
  satisfy pr = PP $ \k t ->
    k $ if pr t
      then single t
      else mempty
  satisfies pr = PP $ \k ts ->
    k $ if all pr ts
      then fromList ts
      else mempty
  oneOf ts = PP $ \k t ->
    k $ if t `elem` ts
      then single t
      else mempty
  noneOf ts = PP $ \k t ->
    k $ if t `elem` ts
      then mempty
      else single t
  end = PP $ \k -> k mempty

instance Monoid s => Category (PP s) where
  id    = PP $ \k -> k mempty
  pa.pb = PP $ \k ->
    pretty pa $ \a ->
    pretty pb $ \b ->
    k $ a <> b

-- }}}



uncons :: (MonadPlus m, Cons' s a) => s -> m (a,s)
uncons = maybe mzero return . L.uncons



-- Parse {{{

runParse :: (MonadPlus m,StreamOf s t) => Parse m s a b -> s -> b -> m (a,s)
runParse p s b = parse p b s

newtype Parse m s a b = Parse
  { parse :: b -> s -> m (a,s)
  }

instance MonadPlus m => Format (Parse m) where
  type Stream (Parse m) s t = StreamOf s t
  type Stream0 (Parse m) s  = StreamOf0 s
  tok t   = Parse $ \f s -> do
    s' <- checkOne t s
    return (f,s')
  anyTok = Parse $ \f s -> do
    (t,s') <- uncons s
    return (f t,s')
  symbol ts = Parse $ \f s -> do
    s' <- checkMany ts s
    return (f,s')
  anySymbol = Parse $ \f -> liftM (first f) . go
    where
    go s = return ([],s) `mplus` case uncons s of
      Just (t,s') -> liftM (first (t:)) $ go s'
      _           -> mzero
  satisfy pr = Parse $ \f s -> do
    (t,s') <- uncons s
    guard (pr t)
    return (f t,s')
  satisfies pr = Parse $ \f -> liftM (first f) . go
    where
    go s = return ([],s) `mplus` case uncons s of
      Just (t,s')
        | pr t -> liftM (first (t:)) $ go s'
      _        -> mzero
  oneOf ts = Parse $ \f s -> do
    (t,s') <- uncons s
    guard $ t `elem` ts
    return (f t,s')
  noneOf ts = Parse $ \f s -> do
    (t,s') <- uncons s
    guard $ not $ t `elem` ts
    return (f t,s')
  end = Parse $ \f s -> if s == mempty
    then return (f,s)
    else mzero

instance (MonadPlus m, Monoid s) => Category (Parse m s) where
  id    = Parse $ \a s -> return (a,s)
  pa.pb = Parse $ \c s -> do
    (b,s') <- parse pa c s
    parse pb b s'

-- }}}

type P t a b = forall r s.
  ( Stream  r s t
  , Stream0 r s
  , Format r
  , Category (r s)
  ) => r s a b

p0 :: P Char a (Char -> a)
p0 = symbol "foo" . satisfy isDigit

p1 :: P Char a (String -> a)
p1 = symbol "Hello, " . anySymbol

p2 :: P Char a (String -> a)
p2 = p1 . tok '.' . end

-- Stream {{{

type StreamOf s t =
  ( Monoid s
  , Cons' s t
  , Eq t
  )

type StreamOf0 s =
  ( Eq s
  , Monoid s
  )

type Cons' s t = Cons s s t t

single :: (Monoid s, Cons' s t) => t -> s
single t = cons t mempty

fromList :: (Monoid s, Cons' s t) => [t] -> s
fromList = foldr cons mempty

checkOne :: (MonadPlus m, Cons' s t,Eq t) => t -> s -> m s
checkOne t s = do
  (t',s') <- uncons s
  guard (t == t')
  return s'

checkMany :: (MonadPlus m, Cons' s t,Eq t) => [t] -> s -> m s
checkMany = go
  where
  go ts s = case ts of
    []    -> return s
    t:ts' -> do
      (t',s') <- uncons s
      guard (t == t')
      go ts' s'

-- }}}

-- Category {{{

(&.) :: Category cat => cat b c -> cat a b -> cat a c
(&.) = (.)
infixr 9 &.

-- }}}

