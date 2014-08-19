{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Unify.Tests where

import Unify

import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.Traversable as T

type Test = Mu (I :+: Add :+: Mul :+: Logic)

-- I {{{

data I a = I Int deriving (Show,Functor,F.Foldable,T.Traversable)

instance Render I where
  render (I x) = show x

instance Equal I where
  equal (I x) (I y) = x == y

instance Uni I Logic

i :: (I :<: f) => Int -> Mu f
i x = inject $ I x

-- }}}

-- Add {{{

data Add a = Add a a deriving (Show,Functor,F.Foldable,T.Traversable)

instance Render Add where
  render (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"

instance Equal Add where
  equal (Add x1 y1) (Add x2 y2) = x1 == x2 && y1 == y2

instance Uni Add Logic where
  uni (Add x1 y1) (Add x2 y2) = unify x1 x2 >> unify y1 y2

add :: (Add :<: f) => Mu f -> Mu f -> Mu f
add x y = inject $ Add x y

-- }}}

-- Mul {{{

data Mul a = Mul a a deriving (Show,Functor,F.Foldable,T.Traversable)

instance Render Mul where
  render (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"

instance Equal Mul where
  equal (Mul x1 y1) (Mul x2 y2) = x1 == x2 && y1 == y2

instance Uni Mul Logic where
  uni (Mul x1 y1) (Mul x2 y2) = unify x1 x2 >> unify y1 y2

mul :: (Mul :<: f) => Mu f -> Mu f -> Mu f
mul x y = inject $ Mul x y

-- }}}

-- Set {{{

data Set a = Set [a] deriving (Show,Functor,F.Foldable,T.Traversable)

instance Render Set where
  render (Set xs) = "{" ++ (intercalate "," $ map show xs) ++ "}"

instance Equal Set where
  equal (Set xs) (Set ys) = and [ or [ x == y | y <- ys ] | x <- xs ]

-- FIXME: remove overlapping cases
instance Uni Set Logic where
  uni (Set xs) (Set ys) =
    let gx = filter (not . isJust . isVar) xs
        gy = filter (not . isJust . isVar) ys in do
      conj [ conj [ disj [ unify x y | y <- ys ] | x <- gx ]
           , conj [ disj [ unify y x | x <- xs ] | y <- gy ]
           ]
  res (Set xs) = do
    xs' <- mapM resolve xs
    return (Set $ nub xs')
      
introSet :: (Render f, Unifiable f v, Set :<: f) => [Mu f] -> GoalM v f (Mu f)
introSet xs = do
  xs' <- mapM resolve xs
  let vs = filter (isJust . isVar) xs'
  msum (return (inject $ Set xs') :
        [ do unify v x
             xsFinal <- nub <$> mapM resolve xs'
             return (inject $ Set xsFinal)
          | v <- vs, x <- xs', v /= x
        ])

-- }}}

-- List {{{

instance Render [] where
  render l = "[" ++ (intercalate "," $ map show l) ++ "]"

instance Equal [] where
  equal = (==)

instance Uni [] Logic where
  uni xs ys = sequence_ $ zipWith unify xs ys

list :: ([] :<: f) => [Mu f] -> Mu f
list xs = inject $ xs

-- }}}

-- Val {{{

data Val t a = Val t deriving (Eq,Show,Functor,F.Foldable,T.Traversable)

instance Show t => Render (Val t) where
  render (Val x) = show x

instance (Eq t,Show t) => Equal (Val t) where
  equal (Val x) (Val y) = x == y

instance (Show t, Eq t) => Uni (Val t) Logic

val :: (Val t :<: f) => t -> Mu f
val x = inject $ Val x

int :: (Val Int :<: f) => Int -> Mu f
int x = inject $ Val x

str :: (Val String :<: f) => String -> Mu f
str x = inject $ Val x

char :: (Val Char :<: f) => Char -> Mu f
char x = inject $ Val x

-- }}}

type CLIST = Mu (I :+: Cons :+: Nil :+: Logic)

-- Cons, Nil {{{

data Cons a = Cons a a deriving (Show,Functor,F.Foldable,T.Traversable)

instance Render Cons where
  render (Cons a d) = "(" ++ show a ++ " . " ++ show d ++ ")"

instance Equal Cons where
  equal (Cons a1 d1) (Cons a2 d2) = a1 == a2 && d1 == d2

instance Uni Cons Logic where
  uni (Cons a1 d1) (Cons a2 d2) = unify a1 a2 >> unify d1 d2

data Nil a = Nil deriving (Show,Functor,F.Foldable,T.Traversable)

instance Render Nil where
  render _ = "()"

instance Equal Nil where
  equal _ _ = True

instance Uni Nil Logic

cons :: (Cons :<: f) => Mu f -> Mu f -> Mu f
cons a d = inject $ Cons a d

nil :: (Nil :<: f) => Mu f
nil = inject Nil

-- }}}

-- MK ops {{{

clist :: (Cons :<: f, Nil :<: f) => [Mu f] -> Mu f
clist = foldr cons nil

appendo :: (Unifiable f v, Cons :<: f, Nil :<: f) => Mu f -> Mu f -> Mu f -> Goal v f
appendo l s ls = disj
  [ do l === nil
       s === ls
  , fresh $ \a d r -> do
      cons a d === l
      cons a r === ls
      appendo d s r
  ]

membero :: (Unifiable f v, Cons :<: f, Nil :<: f) => Mu f -> Mu f -> Goal v f
membero x l = fresh $ \a d -> do
  l === cons a d
  disj [ a === x
       , membero x d
       ]

rembero :: (Unifiable f v, Cons :<: f, Nil :<: f) => Mu f -> Mu f -> Mu f -> Goal v f
rembero x l o = disj
  [ do l === nil
       o === nil
  , fresh $ \d -> do
      l === cons x d
      o === d
  , fresh $ \a d r -> do
      l === cons a d
      rembero x d r
      o === cons a r
  ]

nevero :: (Unifiable f v) => Goal v f
nevero = anyo (fail "nevero")

alwayso :: (Unifiable f v) => Goal v f
alwayso = anyo succeed

foldlo :: (Unifiable f v, Cons :<: f, Nil :<: f) => (Mu f -> Mu f -> Mu f -> Goal v f) -> Mu f -> Mu f -> Mu f -> Goal v f
foldlo rel b as o = disj
  [ do as === nil
       o === nil
  , fresh $ \a d r -> do
      as === cons a d
      rel b a r
      foldlo rel r d o
  ]

foldro :: (Unifiable f v, Cons :<: f, Nil :<: f) => (Mu f -> Mu f -> Mu f -> Goal v f) -> Mu f -> Mu f -> Mu f -> Goal v f
foldro rel b as o = disj
  [ do as === nil
       o === nil
  , fresh $ \a d r -> do
      as === cons a d
      rel a r o
      foldro rel b d r
  ]

-- }}}

-- Oleg Numbers {{{

buildNum :: (Unifiable f v, Cons :<: f, Nil :<: f, I :<: f) => Int -> Mu f
buildNum n
  | n == 0    = nil
  | odd n     = cons (i 1) $ buildNum $ quot (n-1) 2
  | otherwise = cons (i 0) $ buildNum $ quot n 2

poso :: (Unifiable f v, Cons :<: f, Nil :<: f, I :<: f) => Mu f -> Goal v f
poso n = fresh $ \a d -> n === cons a d

greaterThanOneo :: (Unifiable f v, Cons :<: f, Nil :<: f, I :<: f) => Mu f -> Goal v f
greaterThanOneo n = fresh $ \a ad dd -> n === cons a (cons ad dd)

fullAddero :: (Unifiable f v, Cons :<: f, Nil :<: f, I :<: f) => Mu f -> Mu f -> Mu f -> Mu f -> Mu f -> Goal v f
fullAddero b x y r c = disj
  [ i 0 === b >> i 0 === x >> i 0 === y >> i 0 === r >> i 0 === c
  , i 1 === b >> i 0 === x >> i 0 === y >> i 1 === r >> i 0 === c
  , i 0 === b >> i 1 === x >> i 0 === y >> i 1 === r >> i 0 === c
  , i 1 === b >> i 1 === x >> i 0 === y >> i 0 === r >> i 1 === c
  , i 0 === b >> i 0 === x >> i 1 === y >> i 1 === r >> i 0 === c
  , i 1 === b >> i 0 === x >> i 1 === y >> i 0 === r >> i 1 === c
  , i 0 === b >> i 1 === x >> i 1 === y >> i 0 === r >> i 1 === c
  , i 1 === b >> i 1 === x >> i 1 === y >> i 1 === r >> i 1 === c
  ]

addero :: (Unifiable f v, Cons :<: f, Nil :<: f, I :<: f) => Mu f -> Mu f -> Mu f -> Mu f -> Goal v f
addero d n m r = disj
 [ i 0 === d >> nil === m >> n === r
 , i 0 === d >> nil === n >> m === r >> poso m
 , i 1 === d >> nil === m >> addero (i 0) n (clist [i 1]) r
 , i 1 === d >> nil === n >> poso m >> addero (i 0) (clist [i 1]) m r
 , clist [i 1] === n >> clist [i 1] === m >> (fresh $ \a c -> clist [a,c] === r >> fullAddero d (i 1) (i 1) a c)
 , clist [i 1] === n >> genAddero d n m r
 , clist [i 1] === m >> greaterThanOneo n >> greaterThanOneo r >> addero d (clist [i 1]) n r
 , greaterThanOneo n >> genAddero d n m r
 ]

genAddero :: (Unifiable f v, Cons :<: f, Nil :<: f, I :<: f) => Mu f -> Mu f -> Mu f -> Mu f -> Goal v f
genAddero d n m r = fresh $ \a b c e x y z -> do
  cons a x === n
  cons b y === m
  poso y
  cons c z === r
  poso z
  fullAddero d a b c e
  addero e x y z

pluso :: (Unifiable f v, Cons :<: f, Nil :<: f, I :<: f) => Mu f -> Mu f -> Mu f -> Goal v f
pluso n m k = addero (i 0) n m k

type OLEG = Mu (I :+: Cons :+: Nil :+: Logic)

testPlus :: Results OLEG
testPlus = run Nothing Nothing $ \q ->
  fresh $ \x y -> do
    x === buildNum 10
    y === buildNum 10
    pluso x y q

testPlus2 :: Results OLEG
testPlus2 = run Nothing Nothing $ \q ->
  fresh $ \x y -> do
    x === buildNum 10
    y === buildNum 10
    q === buildNum 25
    pluso x y q

class Functor f => OlegNum f where
  toInt :: f Int -> Int 

instance (OlegNum f, OlegNum g) => OlegNum (f :+: g) where
  toInt cp = case cp of
    Inl e -> toInt e
    Inr e -> toInt e

instance OlegNum I where
  toInt (I x) = x

instance OlegNum Nil where
  toInt Nil = 0

instance OlegNum Cons where
  toInt (Cons a d) = a + (2*d)

instance OlegNum Logic where
  toInt _ = error "non-ground"

olegToInt :: (OlegNum f) => Mu f -> Int
olegToInt = foldMu toInt

-- }}}

type MK = Mu (Val Char :+: Val Int :+: Cons :+: Nil :+: Logic)
type SET = Mu (Set :+: Val Char :+: Val Int :+: [] :+: Logic)

-- MK Tests {{{

testMK1 :: Results MK
testMK1 = run Nothing Nothing $ \q -> do
  fresh $ \x y -> do
    disj [ x === int 1
         , x === int 3
         ]
    disj [ y === int 2
         , y === int 4
         ]
    q === clist [x,y]

testMK2 :: Results MK
testMK2 = run Nothing (Just 10) $ \q -> do
  anyo (q === int 1)

testMK3 :: Results MK
testMK3 = run Nothing Nothing $ \q -> do
  onceo $ anyo $ q === int 1

testMK4 :: Results MK
testMK4 = run Nothing Nothing $ \q -> do
  char 'b' === int 3

testMK5 :: Results MK
testMK5 = run Nothing Nothing $ \q -> do
  q === clist [char 'b', int 3]

testMK6 :: Results MK
testMK6 = run Nothing Nothing $ \q -> do
  q === int 1
  q === int 2

-- }}}

-- SET Tests {{{

testSETND :: Results SET
testSETND = run Nothing Nothing $ \q -> do
  fresh $ \x -> do
    s <- introSet [int 1, x]
    q === s

testSETD :: Results SET
testSETD = run Nothing Nothing $ \q -> do
  fresh $ \x -> do
    s <- introSet [int 1, x]
    q === int 2

testSET1 :: Results SET
testSET1 = run Nothing Nothing $ \q -> do
  fresh $ \x y -> do
    x === int 1
    y === int 2
    s <- introSet [x,y]
    q === s

testSET2 :: Results SET
testSET2 = run Nothing Nothing $ \q -> do
  fresh $ \w x y z -> do
    s1 <- introSet [w,int 2]
    x === s1
    s2 <- introSet [int 1,z]
    y === s2
    q === list [x,y]

testSET5 :: Results SET
testSET5 = run Nothing Nothing $ \q -> do
  fresh $ \w x y z -> do
    s1 <- introSet [w,int 2]
    x === s1
    s2 <- introSet [int 1,z]
    y === s2
    x === y
    q === list [x,y]

testSET6 :: Results SET
testSET6 = run Nothing Nothing $ \q -> do
  fresh $ \w x y z -> do
    s1 <- introSet [w,int 2]
    x === s1
    s2 <- introSet [z]
    y === s2
    x === y
    q === list [x,y]

testSET3 :: Results SET
testSET3 = run Nothing Nothing $ \q -> do
  fresh $ \w x y z -> do
    s1 <- introSet [w,int 2]
    x === s1
    s2 <- introSet [int 1,z]
    y === s2
    x === y
    q === list [x,y]

testSET4 :: Results SET
testSET4 = run Nothing Nothing $ \q -> do
  fresh $ \a b c d e -> do
    s1 <- introSet [b,c,int 2]
    a === s1
    s2 <- introSet [e,int 1]
    d === s2
    a === d
    q === list [a,d]

-- }}}

-- CLIST Tests {{{

testFoldlo :: Results CLIST
testFoldlo = run Nothing Nothing $ \q -> do
  fresh $ \a b c d -> do
    a === clist [i 1, i 2]
    b === clist [i 3, i 4]
    c === clist [i 5, i 6]
    d === clist [i 7, i 8]
    foldlo appendo nil (clist [a,b,c,d]) q

testFoldro :: Results CLIST
testFoldro = run Nothing Nothing $ \q -> do
  fresh $ \a b c d -> do
    a === clist [i 1, i 2]
    b === clist [i 3, i 4]
    c === clist [i 5, i 6]
    d === clist [i 7, i 8]
    foldro appendo nil (clist [a,b,c,d]) q

testApp :: Results CLIST
testApp = run Nothing Nothing $ \q -> do
  fresh $ \l1 l2 w x y z -> do
    w === clist [i 1, i 2]
    x === clist [i 2]
    y === clist [i 3]
    z === clist [i 4]
    appendo w x l1
    appendo y z l2
    appendo l1 l2 q

testMem :: Results CLIST
testMem = run Nothing Nothing $ \q -> do
  fresh $ \l -> do
    l === clist [i 1, i 2, i 3]
    membero q l

testRem :: Results CLIST
testRem = run Nothing Nothing $ \q -> do
  fresh $ \x l -> do
    l === clist [i 1, i 2, i 3, i 4]
    rembero x l q

-- }}}

testLength :: Results (Mu (Val Int :+: [] :+: Logic))
testLength = run Nothing Nothing $ \q ->
  fresh $ \x y -> do
    x === list [int 1, int 2, int 3]
    y === list [int 1, int 2]
    x === y
    q === list [x,y]

data Foo a
  = Foo a
  | Bar a
  deriving (Show,Functor,F.Foldable,T.Traversable)

instance Render Foo where
  render f = case f of
    Foo a -> "(Foo " ++ show a ++ ")"
    Bar a -> "(Bar " ++ show a ++ ")"

instance Equal Foo where
  equal x y = case (x,y) of
    (Foo a,Foo b) -> a == b
    (Bar a,Bar b) -> a == b
    _             -> False

instance Uni Foo Logic where
  uni x y = case (x,y) of
    (Foo a,Foo b) -> unify a b
    (Bar a,Bar b) -> unify a b
    _             -> bad x y

foo :: (Foo :<: f) => Mu f -> Mu f
foo x = inject $ Foo x

bar :: (Foo :<: f) => Mu f -> Mu f
bar x = inject $ Bar x

type TEST = Mu (Foo :+: I :+: Cons :+: Nil :+: Logic)

testFoo :: Results TEST
testFoo = run Nothing Nothing $ \q ->
  fresh $ \x y -> do
    x === foo (i 1)
    y === bar (i 1)
    x === y
    q === clist [x,y]

test1 :: Results Test
test1 = run Nothing Nothing (=== i 1)

test2 :: Results Test
test2 = run Nothing Nothing $ \q ->
  fresh $ \x y -> do
    y === i 2
    x === i 1
    disj
      [ q === x
      , q === y
      ]

