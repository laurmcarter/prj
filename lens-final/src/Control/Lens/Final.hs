{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Lens.Final where

import Control.Applicative
import Data.Type.Final
import qualified Prelude as P
import Control.Monad
import Data.Monoid (Monoid(..),(<>))
import GHC.Exts (Constraint)

data a :< b
  = Partial a
  | Other b
  deriving (P.Eq,P.Show)

-- Foo {{{

class Foo r where
  foo  :: Int -> Bool -> r
  _foo :: r -> Either FooI r
  _foo = Right

instance Foo Int where
  foo i _ = i

{-
int :: Int -> Int
int = id
-}

instance Foo r => Foo (Int -> r) where
  foo _ b i = foo i b

_int :: Foo r => (Int -> r) -> Int -> r
_int = id

instance Foo Bool where
  foo _ b = b

{-
bool :: Bool -> Bool
bool = id
-}

instance Foo r => Foo (Bool -> r) where
  foo i _ b = foo i b

_bool :: Foo r => (Bool -> r) -> Bool -> r
_bool = id

instance (Applicative f, Foo r) => Foo ((Int -> f Int) -> f r) where
  foo i b f = foo <$> f i <*> pure b

instance (Applicative f, Foo r) => Foo ((Bool -> f Bool) -> f r) where
  foo i b f = foo <$> pure i <*> f b

{-
instance Monoid m => Foo m where
  foo i b = mempty
-}

data FooI = FooI Int Bool
  deriving (P.Eq,P.Show)

fooI :: FooI -> FooI
fooI = id

instance Foo FooI where
  foo  = FooI
  -- _foo = Left

fooF :: Foo r => FooI -> r
fooF (FooI i b) = foo i b

instance Foo Show where
  foo i b = Show $ \d -> showParen (d P.> 10)
    $ showString "Foo "
    . P.showsPrec 11 i
    . showChar ' '
    . P.showsPrec 11 b

instance Foo r => Foo (Id r) where
  foo i b = Id $ foo i b
  -- _foo  = P.fmap Id . _foo . getId

instance Foo r => Foo (Const r a) where
  foo i b = Const $ foo i b
  -- _foo    = P.fmap Const . _foo . getConst

instance (Foo x, Foo y) => Foo (x,y) where
  foo i b = ( foo i b , foo i b )
  -- _foo (x,y) = do
  --   x' <- _foo x
  --   y' <- _foo y
  --   P.return (x',y')

instance Foo (FooI :< r) where
  foo i b = Partial $ FooI i b

-- }}}

-- Bar {{{

class Bar r where
  bar  :: Double -> Maybe r -> r
  -- _bar :: r -> Either BarI r
  -- _bar = Right

instance Bar Double where
  bar d _ = d

double :: Double -> Double
double = id

instance Bar r => Bar (Maybe r) where
  bar _ m = undefined -- P.fmap barF $ _bar m
  -- _bar  m = m P.>>= _bar

data BarI = BarI Double (Maybe BarI)
  deriving (P.Eq,P.Show)

barI :: BarI -> BarI
barI = id

instance Bar BarI where
  bar  = BarI
  -- _bar = Left

barF :: Bar r => BarI -> r
barF (BarI d mb) = bar d $ P.fmap barF mb

instance Bar Show where
  bar d mb = Show $ \d -> showParen (d P.> 10)
    $ showString "Bar "
    . P.showsPrec 11 d
    . showChar ' '
    . P.showsPrec 11 mb

instance Bar r => Bar (Id r) where
  bar d mb = Id $ bar d $ P.fmap getId mb
  -- _bar     = P.fmap Id . _bar . getId

instance Bar r => Bar (Const r a) where
  bar d mb = Const $ bar d $ P.fmap getConst mb
  -- _bar     = P.fmap Id . _bar . getConst

instance (Bar x, Bar y) => Bar (x,y) where
  bar d mxy =
    ( bar d $ P.fmap fst mxy
    , bar d $ P.fmap snd mxy
    )

{-
  _bar (x,y) = case _bar x of
    Just r -> Just r
    _      -> _bar y
-}

-- }}}

-- Baz Quux {{{

class BQ r where
  baz  :: a -> r a
  quux :: r Int -> r Int -> r Int
  _bazQuux :: r a -> Maybe (BQI a)
  _bazQuux _ = Nothing

instance Monoid m => BQ (FoldMap m) where
  baz a    = FoldMap ($ a)
  quux x y = FoldMap $ (<>) <$> foldMap x <*> foldMap y

data BQI a
  = BazI a
  | QuuxI (BQI Int) (BQI Int)
  deriving (P.Eq,P.Show)

instance BQ BQI where
  baz      = BazI
  quux     = QuuxI
  _bazQuux = Just

instance BQ Show1 where
  baz a = Show1 $ \d -> showParen (d P.> 10)
    $ showString "Foo "
    . P.showsPrec 11 a
  quux x y = Show1 $ \d -> showParen (d P.> 10)
    $ showString "Bar "
    . showsPrec1 x 11
    . showChar ' '
    . showsPrec1 y 11

instance BQ r => BQ (Id1 r) where
  baz a    = Id1 $ baz a
  quux x y = Id1 $ quux (getId1 x) (getId1 y)
  _bazQuux = _bazQuux . getId1

instance (BQ x, BQ y) => BQ (Product x y) where
  baz a    = Product (baz a) (baz a)
  quux x y = Product
    (quux (pFst x) (pFst y))
    (quux (pSnd x) (pSnd y))
  _bazQuux (Product x y) = case _bazQuux x of
    Just r -> Just r
    _      -> _bazQuux y

-- }}}

fb0 :: (Foo r, Bar r) => r
fb0 = bar 3.2 $ Just $ foo 3 False

f0 :: Foo r => r
f0 = foo 3 True

b0 :: Bar r => r
b0 = bar 4.5 $ Just $ bar 1 Nothing

q0 :: BQ r => r Int
q0 = quux (baz 3) (baz 2)

q1 :: BQ r => r Bool
q1 = baz True

q2 :: (BQ r, Foo s) => r s
q2 = baz f0

{-
f1 :: forall r. Foo r => r
f1 = case _foo (foo 3 True :: r) of
  Just (FooI i b) -> foo i b
  _               -> foo 2 False
-}

q3 :: BQ r => r a -> String
q3 r = case _bazQuux r of
  Just (BazI a)    -> "Found a Baz"
  Just (QuuxI x y) -> "Found a Quux"
  _                -> "Didn't find either"

data IR r t where
  Int  :: Int  -> IR r Int
  Bool :: Bool -> IR r Bool
  Neg  :: IR r Int -> IR r Int
  Add  :: IR r Int -> IR r Int -> IR r Int
  Mul  :: IR r Int -> IR r Int -> IR r Int
  IntEQ   :: IR r Int -> IR r Int -> IR r Bool
  If   :: IR r Bool -> IR r a -> IR r a -> IR r a
  Var  :: r t -> IR r t
  Lam  :: (IR r a -> IR r b) -> IR r (a -> b)
  App  :: IR r (a -> b) -> IR r a -> IR r b
  Fix  :: (IR r a -> IR r a) -> IR r a

pushNeg :: IR r a -> IR r a
pushNeg = go
  where
  go :: forall r a. IR r a -> IR r a
  go = \case
    Neg  e     -> case e of
      Neg x     -> go x
      Add x y   -> Add (go $ Neg x) (go $ Neg y)
      Mul x y   -> Mul (go $ Neg x) (go y)
      If  t c a -> If  (go t) (go $ Neg c) (go $ Neg a)
      App f x   -> Neg $ App (go f) (go x)
      Fix f     -> Neg $ Fix $ go . f
      Var x     -> Neg $ Var  x
      Int i     -> Neg (Int i)
    ----
    Add  x y   -> Add (go x) (go y)
    Mul  x y   -> Mul (go x) (go y)
    IntEQ   x y   -> IntEQ  (go x) (go y)
    If   t c a -> If  (go t) (go c) (go a)
    App  f x   -> App (go f) (go x)
    Lam  f     -> Lam $ go . f
    Fix  f     -> Fix $ go . f
    ---- 
    Var  x     -> Var  x
    Int  i     -> Int  i
    Bool b     -> Bool b

class R r where
  int  :: Int -> r Int
  bool :: Bool -> r Bool
  neg  :: r Int -> r Int
  add  :: r Int -> r Int -> r Int
  mul  :: r Int -> r Int -> r Int
  intEq   :: r Int -> r Int -> r Bool
  if_  :: r Bool -> r a -> r a -> r a
  lam  :: (r a -> r b) -> r (a -> b)
  app  :: r (a -> b) -> r a -> r b
  fix  :: (r a -> r a) -> r a

instance R (IR r) where
  int  = Int
  bool = Bool
  neg  = Neg
  add  = Add
  mul  = Mul
  intEq   = IntEQ
  if_  = If
  lam  = Lam
  app  = App
  fix  = Fix

initial :: IR r a -> IR r a
initial = id

final :: R r => IR r a -> r a
final = \case
  Int  i     -> int i
  Bool b     -> bool b
  Neg  x     -> neg (final x)
  Add  x y   -> add (final x) (final y)
  Mul  x y   -> mul (final x) (final y)
  IntEQ   x y   -> intEq  (final x) (final y)
  If   t c a -> if_ (final t) (final c) (final a)
  Var  x     -> x
  Lam  f     -> lam $ final . f . Var
  App  f x   -> app (final f) (final x)
  Fix  f     -> fix $ final . f . Var

instance R (Typed (Pretty Int) ()) where
  int  i     = Typed $ pApp [ str "Int"  , prettys i ]
  bool b     = Typed $ pApp [ str "Bool" , prettys b ]
  neg  x     = Typed $ pApp [ str "Neg"  , typed x   ]
  add  x y   = Typed $ pApp [ str "Add"  , typed x   , typed y ]
  mul  x y   = Typed $ pApp [ str "Mul"  , typed x   , typed y ]
  intEq   x y   = Typed $ pApp [ str "IntEQ"   , typed x   , typed y ]
  if_  t c a = Typed $ pApp [ str "If"   , typed t   , typed c , typed a ]
  lam  f     = Typed $ do
    x <- getUser
    setUser $ x + 1
    let v = str $ "x" ++ P.show x
    pApp [ str "Lam"  , v , typed $ f $ Typed v ]
  app  f x   = Typed $ pApp [ typed f , typed x ]
  fix  f     = Typed $ do
    x <- getUser
    setUser $ x + 1
    let v = str $ "self" ++ P.show x
    pApp [ str "Fix" , v , typed $ f $ Typed v ]

e0 :: R r => r (Int -> Int)
e0 = fix $ \self -> lam $ \x ->
  if_ (intEq x $ int 0)
    (int 1)
    (app self (add x (neg (int 1))))


