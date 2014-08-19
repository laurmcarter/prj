{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Type.STLC where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Functor.Classes
import Data.Monoid
import Prelude hiding (and,or,not,Ordering(..))
import qualified Prelude

class STLC r where
  lam  :: (r a -> r b) -> r (a -> b)
  app  :: r (a -> b) -> r a -> r b
  let_ :: r a -> (r a -> r b) -> r b
  let_ x f = f x

class STBool r where
  bool :: Bool -> r Bool
  cond :: r Bool -> r a -> r a -> r a
  not  :: r Bool -> r Bool
  and  :: r Bool -> r Bool -> r Bool
  or   :: r Bool -> r Bool -> r Bool

class STInt r where
  int         :: Int -> r Int
  add,sub,mul :: r Int -> r Int -> r Int
  isZero      :: r Int -> r Bool
  le,lt,ge,gt :: r Int -> r Int -> r Bool

instance (STInt r, STBool r) => Num (r Int) where
  (+)    = add
  (-)    = sub
  (*)    = mul
  abs x  = cond (le x (int 0)) (negate x) x
  negate = mul (int (-1))
  signum x = cond (gt x (int 0)) (int 1) (cond (lt x (int 0)) (int (-1)) (int 0))
  fromInteger = int . fromInteger

-- Duplicate {{{

duplicate ::
  ( STLC f, STBool f, STInt f
  , STLC g, STBool g, STInt g
  ) => Dup f g a -> (f a, g a)
duplicate = dup

newtype Dup f g a = Dup
  { dup :: (f a,g a)
  } deriving (Eq,Show)

instance (STLC f, STLC g) => STLC (Dup f g) where
  lam f    = Dup
    ( lam $ \x -> fst $ dup $ f $ Dup (x,undefined)
    , lam $ \x -> snd $ dup $ f $ Dup (undefined,x)
    )
  app f x  = Dup
    ( app (fst $ dup f) (fst $ dup x)
    , app (snd $ dup f) (snd $ dup x)
    )
  let_ x f = Dup
    ( let_ (fst $ dup x) $ \x -> fst $ dup $ f $ Dup (x,undefined)
    , let_ (snd $ dup x) $ \x -> snd $ dup $ f $ Dup (undefined,x)
    )

instance (STBool f, STBool g) => STBool (Dup f g) where
  bool b     = Dup ( bool b , bool b )
  cond t c a = Dup
    ( cond (fst $ dup t) (fst $ dup c) (fst $ dup a)
    , cond (snd $ dup t) (snd $ dup c) (snd $ dup a)
    )
  not x      = Dup
    ( not (fst $ dup x)
    , not (snd $ dup x)
    )
  and x y    = Dup
    ( and (fst $ dup x) (fst $ dup y)
    , and (snd $ dup x) (snd $ dup y)
    )
  or x y    = Dup
    ( or (fst $ dup x) (fst $ dup y)
    , or (snd $ dup x) (snd $ dup y)
    )

instance (STInt f, STInt g) => STInt (Dup f g) where
  int n    = Dup ( int n , int n )
  add x y  = Dup
    ( add (fst $ dup x) (fst $ dup y)
    , add (snd $ dup x) (snd $ dup y)
    )
  sub x y  = Dup
    ( sub (fst $ dup x) (fst $ dup y)
    , sub (snd $ dup x) (snd $ dup y)
    )
  mul x y  = Dup
    ( mul (fst $ dup x) (fst $ dup y)
    , mul (snd $ dup x) (snd $ dup y)
    )
  isZero x = Dup
    ( isZero (fst $ dup x)
    , isZero (snd $ dup x)
    )
  le x y   = Dup
    ( le (fst $ dup x) (fst $ dup y)
    , le (snd $ dup x) (snd $ dup y)
    )
  lt x y   = Dup
    ( lt (fst $ dup x) (fst $ dup y)
    , lt (snd $ dup x) (snd $ dup y)
    )
  gt x y   = Dup
    ( gt (fst $ dup x) (fst $ dup y)
    , gt (snd $ dup x) (snd $ dup y)
    )
  ge x y   = Dup
    ( ge (fst $ dup x) (fst $ dup y)
    , ge (snd $ dup x) (snd $ dup y)
    )

-- }}}

-- Eval {{{

evaluate :: Def a -> a
evaluate x = eval x

newtype Eval a = Eval
  { eval :: a
  } deriving (Eq,Show)

instance STLC Eval where
  lam f    = Eval $ eval . f . Eval
  app f x  = Eval $ eval f $ eval x

instance STBool Eval where
  bool       = Eval
  cond t c a = Eval $ if eval t then eval c else eval a
  not x      = Eval $ Prelude.not $ eval x
  and x y    = Eval $ eval x && eval y
  or x y     = Eval $ eval x || eval y

instance STInt Eval where
  int      = Eval
  add x y  = Eval $ eval x + eval y
  sub x y  = Eval $ eval x - eval y
  mul x y  = Eval $ eval x * eval y
  isZero x = Eval $ 0 == eval x
  le x y   = Eval $ eval x <= eval y
  lt x y   = Eval $ eval x <  eval y
  gt x y   = Eval $ eval x >  eval y
  ge x y   = Eval $ eval x >= eval y

-- }}}

-- Render {{{

newtype Render a = Render
  { rndr :: Rndr
  }

instance STLC Render where
  lam f = Render $ par 10 $ prec 0 $ do
    x <- freshVar
    string "\\"
    rndr x
    w_ "."
    rndr $ f x
  app f x  = Render $ par 10 $ rndr f >> space >> rndr x
  let_ x f = Render $ par 10 $ do
    w_ "let"
    v <- freshVar
    rndr v
    _w_ "="
    rndr x
    _w_ "in"
    rndr $ f v

instance STBool Render where
  bool       = Render . rndrs
  cond t c a = Render $ par 10 $ w_ "if" >> rndr t >> _w_ "then" >> rndr c >> _w_ "else" >> rndr a
  not x      = Render $ par 10 $ w_ "not" >> rndr x
  and x y    = Render $ par 3  $ rndr x >> _w_ "&&" >> rndr y
  or x y     = Render $ par 2  $ rndr x >> _w_ "||" >> rndr y

instance STInt Render where
  int      = Render . rndrs
  add x y  = Render $ par 6  $ rndr x >> _w_ "+" >> rndr y
  sub x y  = Render $ par 6  $ rndr x >> _w_ "-" >> rndr y
  mul x y  = Render $ par 7  $ rndr x >> _w_ "*" >> rndr y
  le x y   = Render $ par 10 $ rndr x >> _w_ "<=" >> rndr y
  lt x y   = Render $ par 10 $ rndr x >> _w_ "<"  >> rndr y
  gt x y   = Render $ par 10 $ rndr x >> _w_ ">"  >> rndr y
  ge x y   = Render $ par 10 $ rndr x >> _w_ ">=" >> rndr y
  isZero x = Render $ par 10 $ w_ "zero" >> rndr x

type Def a = forall r. (STLC r, STInt r, STBool r, Num (r Int)) => r a

t0 :: Def (Int -> Int)
t0 = lam $ \x -> add x (mul x (sub x (int 1)))

t1 :: Def Int
t1 = let_ 3 $ \x ->
     let_ 4 $ \y ->
  (x + x) * (y + y)

-- }}}

-- Rndr {{{

type Prec = Int
type Gen  = Int

type Rndr_ = RWS Prec String Gen
type Rndr  = Rndr_ ()

render :: Render a -> String
render m = snd $ execRWS (rndr m) 0 0

rndrs :: Show a => a -> Rndr
rndrs = string . show

freshVar :: Rndr_ (Render a)
freshVar = Render
    . string
    . varId
  <$> fresh

varId :: Int -> String
varId x = c : if d == 0 then "" else show (d - 1)
  where
  (d,m) = divMod x 26
  c     = toEnum $ m + fromEnum 'a'

fresh :: Rndr_ Int
fresh = modifies (+ 1)

par :: Prec -> Rndr -> Rndr
par p m = do
  c <- getPrec
  prec (p + 1)
    $ if c > p
    then do
      char '('
      m
      char ')'
    else m

string :: String -> Rndr
string = tell

_w_ :: String -> Rndr
_w_ m = space >> w_ m

w_ :: String -> Rndr
w_ m = string m >> space

char :: Char -> Rndr
char = string . (:[])

space :: Rndr
space = char ' '

getPrec :: Rndr_ Prec
getPrec = ask

prec :: Prec -> Rndr_ a -> Rndr_ a
prec = local . const

modifies :: (Monad m, Monoid w) => (s -> s) -> RWST r w s m s
modifies f = RWST $ \_ s -> return (s,f s,mempty)

-- }}}

-- Initial / Final {{{

data Exp r a where
  Lam    :: (Exp r a -> Exp r b) -> Exp r (a -> b)
  App    :: Exp r (a -> b) -> Exp r a -> Exp r b
  Let    :: Exp r a -> (Exp r a -> Exp r b) -> Exp r b
  Var    :: r a -> Exp r a
  ----
  Bool   :: Bool -> Exp r Bool
  Cond   :: Exp r Bool -> Exp r a -> Exp r a -> Exp r a
  Not    :: Exp r Bool -> Exp r Bool
  And    :: Exp r Bool -> Exp r Bool -> Exp r Bool
  Or     :: Exp r Bool -> Exp r Bool -> Exp r Bool
  ----
  Int    :: Int -> Exp r Int
  Add    :: Exp r Int -> Exp r Int -> Exp r Int
  Sub    :: Exp r Int -> Exp r Int -> Exp r Int
  Mul    :: Exp r Int -> Exp r Int -> Exp r Int
  IsZero :: Exp r Int -> Exp r Bool
  LE     :: Exp r Int -> Exp r Int -> Exp r Bool
  LT     :: Exp r Int -> Exp r Int -> Exp r Bool
  GT     :: Exp r Int -> Exp r Int -> Exp r Bool
  GE     :: Exp r Int -> Exp r Int -> Exp r Bool

data V = V
  { number :: Int
  , letter :: Int
  } deriving (Eq,Ord)

instance Show V where
  show (V n l) = toEnum (l + fromEnum 'a') : if n == 0 then "" else '_' : show (n - 1)

instance Num V where
  x + y       = toEnum $ fromEnum x + fromEnum y
  x - y       = toEnum $ fromEnum x - fromEnum y
  x * y       = toEnum $ fromEnum x * fromEnum y
  abs         = toEnum . abs    . fromEnum
  signum      = toEnum . signum . fromEnum
  fromInteger = toEnum . fromInteger

normV :: V -> V
normV = toEnum . fromEnum

instance Enum V where
  toEnum   = uncurry V . flip divMod 26
  fromEnum = (+) <$> ((*) 26 <$> number) <*> letter

data WithGen g a = WithGen
  { curGen :: g
  , runGen :: a
  }

type ExpG = Exp (Const V)

instance (Show g, Enum g, Num g) => Show (WithGen g (Exp (Const g) a)) where
  showsPrec d (WithGen g expr) = showParen (d > 10) $ case expr of
    Lam f -> showString "Lam "
      . showsPrec 11
      ( WithGen (g + 1)
        $ f $ Var $ Const g
      )
    Let x f -> let
      v  = Var $ Const g
      g' = g + 1
      in showString "Let "
      . showsPrec 11 ( WithGen g' v )
      . showChar ' '
      . showsPrec 11 ( WithGen g' $ f v )
    App f x    -> str "App "    . sGo 11 (go f) . chr ' ' . sGo 11 (go x)
    Var x      -> str "Var "    . shows (getConst x)
    Bool b     -> str "Bool "   . shows b
    Cond t c a -> str "Cond "   . sGo 11 (go t) . chr ' ' . sGo 11 (go c) . chr ' ' . sGo 11 (go a)
    Not x      -> str "Not "    . sGo 11 (go x)
    And x y    -> str "And "    . sGo 11 (go x) . chr ' ' . sGo 11 (go y)
    Or x y     -> str "Or "     . sGo 11 (go x) . chr ' ' . sGo 11 (go y)
    Int n      -> str "Int "    . shows n
    Add x y    -> str "Add "    . sGo 11 (go x) . chr ' ' . sGo 11 (go y)
    Sub x y    -> str "Sub "    . sGo 11 (go x) . chr ' ' . sGo 11 (go y)
    Mul x y    -> str "Mul "    . sGo 11 (go x) . chr ' ' . sGo 11 (go y)
    IsZero x   -> str "IsZero " . sGo 11 (go x)
    LE x y     -> str "LE "     . sGo 11 (go x) . chr ' ' . sGo 11 (go y)
    LT x y     -> str "LT "     . sGo 11 (go x) . chr ' ' . sGo 11 (go y)
    GT x y     -> str "GT "     . sGo 11 (go x) . chr ' ' . sGo 11 (go y)
    GE x y     -> str "GE "     . sGo 11 (go x) . chr ' ' . sGo 11 (go y)
    where
    go :: forall a. a -> WithGen g a
    go = WithGen g
    sGo :: forall a. Show a => Int -> a -> ShowS
    sGo = showsPrec
    str :: String -> ShowS
    str = showString
    chr :: Char -> ShowS
    chr = showChar

instance STLC (Exp r) where
  lam  = Lam
  app  = App
  let_ = Let

instance STBool (Exp r) where
  bool = Bool
  cond = Cond
  not  = Not
  and  = And
  or   = Or

instance STInt (Exp r) where
  int    = Int
  add    = Add
  sub    = Sub
  mul    = Mul
  isZero = IsZero
  le     = LE
  lt     = LT
  gt     = GT
  ge     = GE

initial :: Exp r a -> Exp r a
initial x = x

pretty :: Exp (Const V) a -> WithGen V (Exp (Const V) a)
pretty = WithGen 0

reify :: r a -> Exp r a
reify = Var

reflect :: (STLC r, STBool r, STInt r) => Exp r a -> r a
reflect expr = case expr of
  Lam    f     -> lam                $ reflect . f . reify
  App    f x   -> app    (reflect f) (reflect x)
  Let    x f   -> let_   (reflect x) $ reflect . f . reify
  Var    x     -> x
  Bool   b     -> bool b
  Cond   t c a -> cond   (reflect t) (reflect c) (reflect a) 
  Not    x     -> not    (reflect x)
  And    x y   -> and    (reflect x) (reflect y)
  Or     x y   -> or     (reflect x) (reflect y)
  Int    n     -> int n
  Add    x y   -> add    (reflect x) (reflect y)
  Sub    x y   -> sub    (reflect x) (reflect y)
  Mul    x y   -> mul    (reflect x) (reflect y)
  IsZero x     -> isZero (reflect x)
  LE     x y   -> le     (reflect x) (reflect y)
  LT     x y   -> lt     (reflect x) (reflect y)
  GT     x y   -> gt     (reflect x) (reflect y)
  GE     x y   -> ge     (reflect x) (reflect y)

-- }}}

instance STLC r => STLC (Free r) where
  lam f = Free $ (lam _,_)

newtype Free r a = Free
  { withFree :: (r a,[Some r])
  }

data Some r = forall a. Some (r a)

