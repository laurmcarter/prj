{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}


import Control.Applicative
import Control.Monad.State
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T

newtype Var = Var
  { var :: Integer
  } deriving (Eq)

instance Show Var where
  show (Var v) = "_." ++ show v

type Subst a = [(Var,Term a)]
data Term f
  = V Var
  | T (f (Term f))

instance Eq (f (Term f)) => Eq (Term f) where
  V x == V y = x == y
  T x == T y = x == y
  _   == _   = False

instance Show (f (Term f)) => Show (Term f) where
  showsPrec d t = showParen (d > 10)
    $ case t of
      V v -> shows v
      T x -> shows x

data Stream a
  = MT
  | a :* Stream a
  -- | a :* Stream a
  | Delay (Stream a)
  deriving (Eq)
infixr 4 :*

instance Show a => Show (Stream a) where
  show s = case s of
    MT       -> ""
    a :* s'  -> show a ++ "\n" ++ show s'
    Delay s' -> unlines $ map ("  " ++) $ lines $ show s'

data Pkg a = Pkg
  { pkgCount :: Integer
  , pkgSubst :: Subst a
  }

takeStream :: Int -> Stream a -> Stream a
takeStream i s
  | i <= 0    = MT
  | otherwise = case s of
    MT       -> MT
    a :* s'  -> ((:*) $! a) $! takeStream (i-1) s'
    Delay s' -> takeStream i s'

instance Functor Stream where
  fmap f s = case s of
    MT       -> MT
    a :* s'  -> f a :* fmap f s'
    Delay s' -> Delay $ fmap f s'

instance Applicative Stream where
  pure = return
  (<*>) = ap

instance Monad Stream where
  return = (:* MT)
  s >>= f = case s of
    MT       -> mzero
    a :* s'  -> f a `mplus` (s' >>= f)
    Delay s' -> Delay $ s' >>= f

instance MonadPlus Stream where
  mzero = MT
  mplus s1 s2 = case s1 of
    MT       -> s2
    a :* s'  -> a :* (s' `mplus` s2)
    Delay s' -> Delay $ s2 `mplus` s'

instance Alternative Stream where
  empty = mzero
  (<|>) = mplus

newtype Goal p a = Goal
  { runGoal :: Pkg p -> Stream (Pkg p,a)
  }

instance Functor (Goal p) where
  fmap f g = Goal $ \p -> fmap (fmap f) $ runGoal g p

instance Applicative (Goal p) where
  pure = return
  (<*>) = ap

instance Monad (Goal p) where
  return a = Goal $ \p -> return (p,a)
  g >>= f  = Goal $ \p -> do
    (p',a) <- runGoal g p
    runGoal (f a) p'

instance Alternative (Goal p) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (Goal p) where
  mzero = Goal $ \_ -> mzero
  mplus g1 g2 = Goal $ \p -> runGoal g1 p `mplus` runGoal g2 p

getPkg :: Goal p (Pkg p)
getPkg = Goal $ \p -> return (p,p)

initPkg :: Pkg f
initPkg = Pkg 0 []


data Pair a = Pair a a deriving (Eq,Functor,Foldable,Traversable)

instance Show a => Show (Pair a) where
  show (Pair a b) = "(" ++ show a ++ " . " ++ show b ++ ")"



class Zip t where
  zipF :: Applicative f => (a -> b -> f c) -> t a -> t b -> Maybe (f (t c))

instance Zip Pair where
  zipF f (Pair a b) (Pair c d) = Just $ Pair <$> f a c <*> f b d

{- Example instance for lists
instance Zip [] where
  zipF f xs ys = case (xs,ys) of
    ([],[])           -> Just $ pure []
    (x : xs',y : ys') -> do
      rest <- zipF f xs' ys'
      return $ (:) <$> f x y <*> rest
    _                 -> Nothing
-}


 
unit :: Goal p ()
unit = return ()

disj :: Goal p a -> Goal p a -> Goal p a
disj = mplus

conj :: Goal p () -> Goal p () -> Goal p ()
conj g1 g2 = Goal $ \p ->
  (fst <$> runGoal g1 p) >>= runGoal g2

delay :: Goal p () -> Goal p ()
delay g = Goal $ \p -> Delay $ runGoal g p

callFresh :: (Var -> Goal p a) -> Goal p a
callFresh f = Goal go
  where
  go (Pkg c s) = runGoal (f $ Var c) p'
    where
    p' = Pkg { pkgCount = c + 1 , pkgSubst = s }

unify :: (Zip f, Eq (f (Term f)))
  => Term f -> Term f
  -> Goal f ()
unify u v = do
  s <- pkgSubst <$> getPkg
  case (walk u s,walk v s) of
    (u',v') | u' == v' -> unit
    (V x,v') -> modifySubst (extend x v') unit
    (u',V x) -> modifySubst (extend x u') unit
    (T u',T v') -> case zipF unify u' v' of
      Just g -> void g
      _      -> mzero

type MK f = (Zip f, Eq (f (Term f)))

(≡) :: MK f => Term f -> Term f -> Goal f ()
(≡) = unify

walk :: Term f -> Subst f -> Term f
walk u s = case u of
  V v  -> case lookup v s of
    Just t -> walk t s
    _      -> u
  T a -> T a

modifySubst :: (Subst p -> Subst p) -> Goal p a -> Goal p a
modifySubst f g = Goal $ \p -> runGoal g $ p { pkgSubst = f $ pkgSubst p }

extend :: Var -> Term p -> Subst p -> Subst p
extend x t s = (x,t) : s

data (f :*: g) a = L (f a) | R (g a) deriving (Eq,Functor,Foldable,Traversable)
infixr 4 :*:

instance (Show (f a), Show (g a)) => Show ((f :*: g) a) where
  showsPrec d t = case t of
    L x -> showsPrec d x
    R y -> showsPrec d y

instance (Zip f, Zip g) => Zip (f :*: g) where
  zipF f e1 e2 = case (e1,e2) of
    (L a,L b) -> fmap (fmap L) $ zipF f a b
    (R a,R b) -> fmap (fmap R) $ zipF f a b
    _         -> Nothing

data C r a = C { getC :: r } deriving (Eq,Functor,Foldable,Traversable)

newtype Symbol a = Sym
  { sym :: String
  } deriving (Eq,Functor,Foldable,Traversable)

instance Show (Symbol a) where
  show (Sym s) = s

instance Zip Symbol where
  zipF f (Sym s1) (Sym s2) = Just $ pure $ Sym s1

instance Show r => Show (C r a) where
  showsPrec d (C r) = showsPrec d r

instance Zip (C r) where
  zipF f (C r1) (C r2) = Just $ pure $ C r1

type U = Symbol :*: C () :*: Pair

pattern S s   = T (L (Sym s))
pattern Unit  = T (R (L (C ())))
pattern P a b = T (R (R (Pair a b)))

{-
Injection into type universe:
Term U
  := V <var>
   | T (L (C <string>))
   | T (R (L (C ())))
   | T (R (R (Pair <term>)))
-}

hotDogs :: Term U -> Goal U ()
hotDogs dish =
  disj (dish ≡ S "dog")
  $ callFresh $ \res ->
    conj (dish ≡ P (S "hot") (V res))
      $ hotDogs $ V res

appendo :: Term U -> Term U -> Term U -> Goal U ()
appendo x y r =
  disj
    (conj
      (x ≡ Unit)
      (r ≡ y))
    (callFresh $ \a ->
      (callFresh $ \d ->
        (conj
          (x ≡ P (V a) (V d))
          (callFresh $ \res ->
            (conj
              (r ≡ P (V a) (V res))
              (appendo (V d) y (V res)))))))

run :: Goal U () -> Stream (Term U)
run g = reifyFirst . pkgSubst . fst <$> runGoal g initPkg

demo :: Int -> Stream (Term U)
demo n = takeStream n
  $ run $ callFresh $ \q ->
    hotDogs $ V q

demo2 :: Int -> Stream (Term U)
demo2 n = takeStream n
  $ run $
    callFresh $ \q ->
    callFresh $ \x ->
    callFresh $ \y ->
    conj
      (V q ≡ P (V x) (V y))
      (appendo (V x) (V y)
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d")
        (P (S "a") (P (S "b") (P (S "c") (P (S "d") Unit)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))


reifyFirst :: Subst U -> Term U
reifyFirst s = walkStar v $ reifyR v
  where
  v = walkStar (V $ Var 0) s

walkStar :: Term U -> Subst U -> Term U
walkStar w s = case walk w s of
  V v -> V v
  T t -> T $ fmap (`walkStar` s) t

reifyR :: Term U -> Subst U
reifyR v = execState (go v) []
  where
  go v = do
    s <- get
    let n = S $ reifyName $ length s
    case walk v s of
      V x -> put $ (x,n) : s
      T x -> F.traverse_ go x

reifyName :: Int -> String
reifyName i = "_." ++ show i

main :: IO ()
main = print $ demo2 1000
  -- seq (demo2 1000) $ return ()

