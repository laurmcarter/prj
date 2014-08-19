{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Interpreter.Linear where

import Control.Arrow (first,second,(***))
import Data.Proxy
import Data.Typeable
import Data.String.UTF8
import Data.List (intercalate)
import Data.Word
import Numeric.Lens (hex)
import Data.Bits.Lens (byteAt)

class Lin (r :: Nat -> Bool -> [Maybe Nat] -> [Maybe Nat] -> * -> *) where
  λ_   :: VarOk tf var
       => (LinVar r v a -> r (S v) tf (Just v ': hi) (var ': ho) b)
       -> r v tf hi ho (a :-<> b)
  (#~) :: r v tf0 hi h  (a :-<> b)
       -> r v tf1 h ho a
       -> r v (tf0 || tf1) hi ho b

  λ    :: (RegVar r a -> r v tf hi ho b) -> r v tf hi ho (a -> b)
  (#)  :: r v tf0 hi ho (a -> b) -> r v tf1 ho ho a -> r v tf0 hi ho b

  one    :: r v False h h One
  letOne :: r v tf0 hi h One -> r v tf1 h h0 a -> r v (tf0 || tf1) hi ho a

  (.*.) :: r v tf0 hi h  a
        -> r v tf1 h  h0 b
        -> r v (tf0 || tf1) hi ho (a :*: b)
  letStar :: (VarOk tf1 v0, VarOk tf1 v1)
          => r v tf0 hi h (a :*: b)
          -> (  LinVar r v a
             -> LinVar r (S v) b
             -> r (S (S v)) tf1
                (Just v ': Just (S v) ': h)
                (v0     ': v1         ': ho)
                c
             )
          -> r v (tf0 || tf1) hi ho c

  (!)     :: r v tf h h a -> r v False h h (Bang a)
  letBang :: r v tf0 hi h (Bang a)
          -> (RegVar r a -> r v tf1 h ho b)
          -> r v (tf0 || tf1) hi ho b

  top     :: r vid True h h Top

  (<&>)   :: r v tf0 hi h0 a
          -> r v tf1 hi h1 b
          -> r v (AddAnd tf0 tf1 h0 h1) hi (Intersect h0 h1) (a :&: b)

  pi1     :: r v tf hi ho (a :&: b) -> r v tf hi ho a
  pi2     :: r v tf hi ho (a :&: b) -> r v tf hi ho b

  inl     :: r v tf hi ho a -> r v tf hi ho (a :+: b)
  inr     :: r v tf hi ho b -> r v tf hi ho (a :+: b)

  letPlus :: (VarOk tf1 v1, VarOk tf2 v2)
          => r v tf0 hi h (a :+: b)
          -> (LinVar r v a -> r (S v) tf1 (Just v ': h) (v1 ': ho1) c)
          -> (LinVar r v b -> r (S v) tf2 (Just v ': h) (v2 ': ho2) c)
          -> r v (tf0 || AddAnd tf1 tf2 ho1 ho2) hi (Intersect ho1 ho2) c

  abort   :: r v tf hi ho Zero -> r v True hi ho a
infixl 1 #~
infixl 1 #

newtype a :-<> b = Lolli { unLolli :: a -> b }
infixr 4 :-<>
data a :*: b = Tensor a b
data One = One
data a :&: b = Pair a b
data Top = Top
data a :+: b = Inl a | Inr b
data Zero
newtype Bang a = Bang { unBang :: a }

type LinVar (r :: Nat -> Bool -> [Maybe Nat] -> [Maybe Nat] -> * -> *) vid a
  = forall v i o. Consume vid i o => r v False i o a

type RegVar (r :: Nat -> Bool -> [Maybe Nat] -> [Maybe Nat] -> * -> *) a
  = forall v h. r v False h h a

type Def tf a = forall r v h.
  ( Lin r
  , Intersect h h ~ h
  , (h :<: h) ~ True
  ) => r v tf h h a

def :: Def tf a -> Def tf a
def x = x

type LinearDef a = Def False a
linearDef :: LinearDef a -> LinearDef a
linearDef x = x

type AffineDef a = Def True a
affineDef :: AffineDef a -> AffineDef a
affineDef x = x



($.) :: (a -> b) -> a -> b
($.) = ($)
infixr 2 $.

(.&) :: (b -> c) -> (a -> b) -> a -> c
(.&) = (.)
infixr 0 .&

-- EQ {{{

class EQ (x :: k) (y :: k) (b :: Bool) | x y -> b
instance EQ x x True
instance (b ~ False) => EQ x y b

-- }}}


-- || && {{{

type family (x :: Bool) || (y :: Bool) :: Bool where
  True  || y     = True
  False || True  = True
  False || False = False
infixr 5 ||


type family (x :: Bool) && (y :: Bool) :: Bool where
  False && y     = False
  True  && False = False
  True  && True  = True
infixr 6 &&

-- }}}


-- Consume {{{

class Consume  (v :: Nat)  (i :: [Maybe Nat]) (o :: [Maybe Nat]) | v i -> o
class Consume1 (b :: Bool) (v :: Nat) (x :: Nat)
  (i :: [Maybe Nat]) (o :: [Maybe Nat]) | b v x i -> o

instance Consume v i o => Consume v (Nothing ': i) (Nothing ': o)
instance (EQ v x b, Consume1 b v x i o) => Consume v (Just x ': i) o

instance Consume1 True  v x i (Nothing ': i)
instance Consume v i o => Consume1 False v x i (Just x ': o)

-- }}}


-- :<: {{{

type family (x :: [Maybe Nat]) :<: (y :: [Maybe Nat]) :: Bool where
  '[]             :<: '[]             = True
  (Nothing ': xs) :<: (Nothing ': ys) = xs :<: ys
  (Nothing ': xs) :<: (Just y  ': ys) = xs :<: ys
  (Just x  ': xs) :<: (Just x  ': ys) = xs :<: ys
  (Just x  ': xs) :<: (v       ': ys) = False -- Just y (x /= y), Nothing

-- }}}


-- Intersect {{{

type family Intersect (xs :: [Maybe Nat]) (ys :: [Maybe Nat]) :: [Maybe Nat] where
  Intersect '[]             '[]             = '[]
  Intersect (Nothing ': xs) (Nothing ': ys) = Nothing ': Intersect xs ys
  Intersect (Nothing ': xs) (Just y  ': ys) = Nothing ': Intersect xs ys
  Intersect (Just x  ': xs) (Nothing ': ys) = Nothing ': Intersect xs ys
  Intersect (Just x  ': xs) (Just x  ': ys) = Just x  ': Intersect xs ys

-- }}}


-- AddAnd When {{{

type family AddAnd
  (tf0 :: Bool)       (tf1 :: Bool)
  (h0 :: [Maybe Nat]) (h1 :: [Maybe Nat])
  :: Bool where
  AddAnd False False h  h  = False
  AddAnd False True  h0 h1 = When (h0 :<: h1) False
  AddAnd True  False h0 h1 = When (h1 :<: h0) False
  AddAnd True  True  h0 h1 = True

type family When (x :: Bool) (y :: Bool) :: Bool where
  When True y = y

-- }}}


-- VarOk {{{

class    VarOk (tf :: Bool) (v :: Maybe Nat)
instance VarOk True  (Just v)
instance VarOk True  Nothing
instance VarOk False Nothing

-- }}}


-- Nat {{{

data Nat
  = Z
  | S Nat
  deriving (Eq,Show)

class Natural (n :: Nat) where
  natVal :: Proxy n -> Int

instance Natural Z where
  natVal _ = 0

instance Natural n => Natural (S n) where
  natVal _ = 1 + natVal (Proxy :: Proxy n)

-- Printing for Nat and Env types

showNat :: Natural n => Proxy n -> String
showNat = show . natVal

data Env (h :: [Maybe Nat]) = Env

instance Show (Env '[]) where
  show _ = "{}"

instance Show (Env h) => Show (Env (Nothing ': h)) where
  show _ = "{X" ++ rest
    where
    rest = case show (Env :: Env h) of
      _:"}" -> "}"
      s     -> ',' : tail s

instance (Natural n, Show (Env h)) => Show (Env (Just n ': h)) where
  show _ = "{" ++ showNat (Proxy :: Proxy n) ++ rest
    where
    rest = case show (Env :: Env h) of
      _:"}" -> "}"
      s     -> ',' : tail s

-- }}}


t0 :: LinearDef ((a :-<> b) :-<> a :-<> b)
t0 = λ_ $ \f -> λ_ $ \x -> f #~ x

t1 :: LinearDef ((a -> (a :-<> b)) :-<> (a -> b))
t1 = λ_ $ \f -> λ $ \x -> f # x #~ x

type B = One :+: One
tt :: LinearDef B
tt = inl one
ff :: LinearDef B
ff = inr one

{-
cond :: LinearDef (B :-<> ((a :&: a) :-<> a))
cond = λ_ $ \b -> λ_ $ \tf -> letPlus b _ _
  -- (\x -> letOne x $ pi1 tf)
  -- (\x -> letOne x $ pi2 tf)
-}

foo :: LinearDef One
foo = letOne one one

-- Eval {{{

newtype Eval (v :: Nat) (tf :: Bool) (hi :: [Maybe Nat]) (ho :: [Maybe Nat]) a = Eval { eval_ :: a }

eval :: Eval Z tf '[] '[] a -> a
eval = eval_

instance Lin Eval where
  λ_   f = Eval $ Lolli $ \x -> eval_ $ f $ Eval x
  f #~ x = Eval $ unLolli (eval_ f) (eval_ x)

  λ    f = Eval $ \x -> eval_ $ f $ Eval x
  f #  x = Eval $ eval_ f $ eval_ x

  (!) = Eval . Bang . eval_
  letBang x f = Eval $ eval_ $ f' $ eval_ x
    where
    f' (Bang x) = f $ Eval x

  x .*. y = Eval $ Tensor (eval_ x) (eval_ y)
  letStar xy f = Eval $ eval_ $ f' $ eval_ xy
    where f' (Tensor x y) = f (Eval x) (Eval y)

  one = Eval One
  letOne x y = Eval $ eval_ $ (\One -> y) $ eval_ x

  top = Eval Top
  
  x <&> y = Eval $ Pair (eval_ x) (eval_ y)
  pi1 = Eval . (\(Pair x _) -> x) . eval_
  pi2 = Eval . (\(Pair _ y) -> y) . eval_

  inl = Eval . Inl . eval_
  inr = Eval . Inr . eval_
  letPlus xy fl fr = case eval_ xy of
    Inl x -> Eval $ eval_ $ fl $ Eval x
    Inr y -> Eval $ eval_ $ fr $ Eval y

  abort x = Eval $ error "abort"

-- }}}

-- Render {{{

newtype P (v :: Nat) (tf :: Bool) (hi :: [Maybe Nat]) (ho :: [Maybe Nat]) a = P
  { gShowsPrec :: Int -> ShowG
  }

render :: P Z tf '[] '[] a -> IO ()
render p = putStrLn $ snd $ gShowsPrec p 0 (0,"")

type ShowG = (Int,String) -> (Int,String)

gShows :: P v tf hi ho a -> ShowG
gShows p = gShowsPrec p 0

gsPar :: Bool -> ShowG -> ShowG
gsPar b p = if b then gsChar '(' . p . gsChar ')' else p

gsChar :: Char -> ShowG
gsChar = liftShowS . showChar

liftShowS :: ShowS -> ShowG
liftShowS = second

gsStr :: String -> ShowG
gsStr = liftShowS . showString

gsBind :: (Int -> ShowG) -> ShowG
gsBind f (g,s) = f g (g+1,s)

gsFresh :: String -> (String -> ShowG) -> ShowG
gsFresh x f = gsBind $ \i -> f $ x ++ subscript i

gsStr_ :: String -> Int -> ShowG
gsStr_ = const . gsStr

subscript :: Int -> String
subscript n
  | n >= 10
  = uncurry (++) $ subscript *** subscript $ n `divMod` 10

  | n >= 0
  = ["₀₁₂₃₄₅₆₇₈₉" !! n]

  | otherwise
  = "₋" ++ subscript (negate n)
  where

instance Lin P where
  λ_   f = P $ \d -> gsPar (d > 10)
    $ gsStr "λ->"
   .& gsFresh "l" $ \(("\ESC[4m" ++).(++ "\ESC[m") -> x) ->
      gsStr (" "++x++". ")
    . gShows (f $ P $ gsStr_ x)

  f #~ x = P $ \d -> gsPar (d > 10)
    $ gShowsPrec f 11 . gsStr " " . gShowsPrec x 11

  λ    f = P $ \d -> gsPar (d > 10)
    $ gsStr "λ"
   .& gsFresh "r" $ \x ->
      gsStr (" "++x++". ")
    . gShows (f $ P $ gsStr_ x)

  f #  x = P $ \d -> gsPar (d > 10)
    $ gShowsPrec f 11 . gsStr " " . gShowsPrec x 11

  (!) a = P $ \d -> gsPar (d > 10)
    $ gsStr "! "
    . gShowsPrec a 11 
  letBang x f = P $ \d -> gsPar (d > 10)
    $ gsStr "letBang"
   .& gsFresh "x" $ \v ->
      gsStr (" "++v++" = ")
    . gShows x
    . gsStr " in "
    . gShows (f $ P $ gsStr_ v)

  x .*. y = P $ \d -> gsPar (d > 10)
    $ gShowsPrec x 11
    . gsStr " .*. "
    . gShowsPrec y 11
  letStar xy f = P $ \d -> gsPar (d > 10)
    $ gsStr "letStar"
   .& gsFresh "x" $ \vx ->
      gsFresh "y" $ \vy ->
      gsStr (" <"++vx++","++vy++"> = ")
    . gShows xy
    . gsStr " in "
    . gShows (f (P $ gsStr_ vx) (P $ gsStr_ vy))

  one = P $ gsStr_ "1"
  letOne x y = P $ \d -> gsPar (d > 10)
    $ gsStr "letOne"
   .& gsFresh "x" $ \v ->
      gsStr (" "++v++" = ")
    . gShows x
    . gsStr " in "
    . gShows y

  top = P $ gsStr_ "Top"
  
  x <&> y = P $ \d -> gsPar (d > 10)
    $ gsStr "<"
    . gShowsPrec x 11
    . gsStr ","
    . gShowsPrec y 11
    . gsStr ">"
  pi1 xy = P $ \d -> gsPar (d > 10)
    $ gsStr "pi1 "
    . gShowsPrec xy 11
  pi2 xy = P $ \d -> gsPar (d > 10)
    $ gsStr "pi2 "
    . gShowsPrec xy 11

  inl xy = P $ \d -> gsPar (d > 10)
    $ gsStr "inl "
    . gShowsPrec xy 11
  inr xy = P $ \d -> gsPar (d > 10)
    $ gsStr "inr "
    . gShowsPrec xy 11
  letPlus xy fl fr = P $ \d -> gsPar (d > 10)
    $ gsStr "case "
   .& gsFresh "x" $ \vx ->
      gsFresh "y" $ \vy ->
      gShows xy
    . gsStr (" of { inl "++vx++" -> ")
    . gShows (fl $ P $ gsStr_ vx)
    . gsStr (" } { inr "++vy++" -> ")
    . gShows (fr $ P $ gsStr_ vy)
    . gsStr " }"

  abort _x = P $ gsStr_ "abort"


-- }}}

test :: IO ()
test = do
  render t0
  putStrLn
    $ unLolli
    ( eval $ t0 #~ λ_ $. \x -> x)
    "I was passed to a real function" 



{-
-- ANSI Effects {{{

data Effect
  = Normal
  | Bold
  | Faint
  | Italic
  | Underline
  | Blink
  | Negative
  | CrossOut
  | NoBoldFaint
  | NoItalic
  | NoUnderline
  | NoBlink
  | NoNegative
  | NoCrossOut
  | FG Color8
  | FG256 Color256
  | NoFG
  | BG Color8
  | BG256 Color256
  | NoBG
  | Frame
  | Encircle
  | Overline
  | NoFrameEncircle
  | NoOverline
  deriving (Eq,Show)

data D6
  = D0
  | D1
  | D2
  | D3
  | D4
  | D5
  deriving (Eq,Show,Enum)

d6Digits :: [D6] -> Int
d6Digits = go 0
  where
  go acc ds = case ds of
    []    -> acc
    d:ds' -> go (fromEnum d + 6 * acc) ds'

data Color8
  = Black
  | White
  | C8 D6
  deriving (Eq,Show)

black, white, red, green, yellow, blue, magenta, cyan :: Color8
black   = Black
white   = White
red     = C8 D0
green   = C8 D1
yellow  = C8 D2
blue    = C8 D3
magenta = C8 D4
cyan    = C8 D5

data Color256
  = ColorCube D6 D6 D6
  | Grayscale Int -- ^ 0 - 23
  | C256_8 Color8
  deriving (Eq,Show)

eff :: [Effect] -> String -> String
eff es s = ansi es $ \o ->
  ansi [Normal] $ \c ->
    o ++ s ++ c

class ANSICode t r | t -> r where
  ansi :: t -> (r -> String) -> String

instance ANSICode Color8 Int where
  ansi c8 f = f $ case c8 of
    Black -> 0
    C8 c  -> 1 + fromEnum c
    White -> 7

instance ANSICode Color256 Int where
  ansi c256 f = case c256 of
    C256_8 c8       -> ansi c8 f
    ColorCube r g b -> f $ 16  + d6Digits [r,g,b]
    Grayscale g     -> f $ 232 + n
      where
      n | g < 0     = 0
        | g >= 24   = 24
        | otherwise = g

instance ANSICode Effect Int where
  ansi eff f = case eff of
    FG c8           -> ansi c8   $ f . (+ 30)
    FG256 c256      -> ansi c256 $ ("38;5;" ++) . f
    BG c8           -> ansi c8   $ f . (+ 40)
    BG256 c256      -> ansi c256 $ ("48;5;" ++) . f
    Normal          -> f 0
    Bold            -> f 1
    Faint           -> f 2
    Italic          -> f 3
    Underline       -> f 4
    Blink           -> f 5
    Negative        -> f 7
    CrossOut        -> f 9
    NoBoldFaint     -> f 22
    NoItalic        -> f 23
    NoUnderline     -> f 24
    NoBlink         -> f 25
    NoNegative      -> f 27
    NoCrossOut      -> f 29
    NoFG            -> f 39
    NoBG            -> f 49
    Frame           -> f 51
    Encircle        -> f 52
    Overline        -> f 53
    NoFrameEncircle -> f 54
    NoOverline      -> f 55

instance ANSICode [Effect] String where
  ansi es f = f
    $ ("\ESC[" ++) $ (++ "m")
    $ intercalate ";"
    $ map (flip ansi show) es

-- }}}
-}

