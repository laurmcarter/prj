-- Pragmas {{{
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- }}}

module Data.Vec.Unary where

-- Imports {{{

import Prelude hiding
  (head,tail,init,last,reverse,replicate)
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens hiding ((<.>),index)
import Data.Proxy
import Data.Void
-- import Data.Number.Fin.Integer
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable (Foldable(..))
import qualified Data.Foldable as F
import Data.Distributive (Distributive(..))
import Data.Semigroup.Foldable (Foldable1(..))
import Data.Semigroup.Traversable (Traversable1(..))
import Data.Functor.Bind (Apply(..),Bind(..))
import qualified Data.Functor.Rep as R

import qualified Linear as Lin
import qualified Linear.Affine as Lin
import qualified Data.AdditiveGroup as VS
import qualified Data.AffineSpace as VS
import qualified Data.Basis as VS
import qualified Data.Cross as VS
import qualified Data.VectorSpace as VS

-- }}}

data Vec (n :: UN) a where
  Nil :: Vec Z a
  (:*) :: Nat n => a -> Vec n a -> Vec (S n) a

type Matrix m n a = Vec m (Vec n a)

-- FromList {{{

class FromList (ns :: [UN]) where
  type NDimVec ns (a :: *) :: *
  type Lists   ns (a :: *) :: *
  dimNFromList_ :: NatsProxy ns -> Proxy a -> Lists ns a -> Maybe (NDimVec ns a)
instance FromList '[] where
  type NDimVec '[] a = a
  type Lists   '[] a = a
  dimNFromList_ _ _ a = Just a
instance (Nat n, FromList ns) => FromList (n ': ns) where
  type NDimVec (n ': ns) a = Vec n (NDimVec ns a)
  type Lists   (n ': ns) a = [Lists ns a]
  dimNFromList_ (Proxy :: NatsProxy (n ': ns)) (pa :: Proxy a) as = fromList =<< mvs
    where
    pn :: NatsProxy ns
    pn = Proxy
    mvs :: Maybe [NDimVec ns a]
    mvs = mapM (dimNFromList_ pn pa) as

type NatsProxy (ns :: [UN]) = Proxy ns

fromList0 :: forall a. a -> Maybe a
fromList0 = dimNFromList_ (Proxy :: NatsProxy '[]) (Proxy :: Proxy a)

fromList1 :: Nat n => [a] -> Maybe (NDimVec '[n] a)
fromList1 = fromList

fromList2 :: forall m n a. (Nat m, Nat n) => [[a]] -> Maybe (NDimVec [m,n] a)
fromList2 = dimNFromList_ (Proxy :: NatsProxy [m,n]) (Proxy :: Proxy a)

fromList3 :: forall l m n a. (Nat l, Nat m, Nat n) => [[[a]]] -> Maybe (NDimVec [l,m,n] a)
fromList3 = dimNFromList_ (Proxy :: NatsProxy [l,m,n]) (Proxy :: Proxy a)

fromList4 :: forall k l m n a. (Nat k, Nat l, Nat m, Nat n) => [[[[a]]]] -> Maybe (NDimVec [k,l,m,n] a)
fromList4 = dimNFromList_ (Proxy :: NatsProxy [k,l,m,n]) (Proxy :: Proxy a)

-- }}}

-- Helpers {{{

-- Convenient syntax for ending vectors
(*:) :: a -> a -> Vec (S (S Z)) a
a *: b = a :* b :* Nil

infixr 4 :*
infixr 4 *:

class AsProxy x (a :: k) | x -> a where
  asProxy :: x -> Proxy a
  asProxy _ = Proxy

instance AsProxy (Vec n a) n

data UN
  = Z
  | S UN
  deriving (Eq,Ord)

instance Show UN where
  showsPrec d n = showParen (d > 10)
    $ showString "UN "
    . shows (fromEnum n)

instance Num UN where
  m + n = case m of
    Z    -> n
    S m' -> S (m' + n)
  m * n = case m of
    Z    -> Z
    S m' -> n + (m' * n)
  m - n = case (m,n) of
    (_,Z) -> m
    (S m',S n') -> m' - n'
    _ -> n
  negate = id
  abs = id
  signum _ = S Z
  fromInteger n
    | n == 0
    = Z
    | n > 0
    = S $ fromInteger $ pred n
    | otherwise
    = fromInteger $ negate n

instance Enum UN where
  toEnum = fromInteger . toEnum
  fromEnum n = case n of
    Z -> 0
    S n' -> 1 + fromEnum n'

fromList :: Nat n => [a] -> Maybe (Vec n a)
fromList = preview _List

toList :: Nat n => Vec n a -> [a]
toList = review _List

replicate :: Nat n => a -> Vec n a
replicate = generate . const

generate :: forall n a. Nat n => (Int -> a) -> Vec n a
generate = generate' $ nat p
  where
  p :: Proxy n
  p = Proxy

-- }}}

-- Construction {{{

empty :: Vec Z a
empty = Nil

singleton :: a -> Vec One a
singleton a = a :* Nil

-- }}}

-- Destruction {{{

dim :: (Nat n, Num x, Enum x) => Vec n a -> x
dim = nat . asProxy

head :: Vec (S n) a -> a
head (a :* _) = a

tail :: Vec (S n) a -> Vec n a
tail (_ :* v) = v

init :: Vec (S n) a -> Vec n a
init (a :* v) = case v of
  Nil -> Nil
  _ :* _ -> a :* init v

last :: Vec (S n) a -> a
last (a :* v) = case v of
  Nil -> a
  _ :* _ -> last v

member :: Eq a => Vec n a -> a -> Bool
member v a = case v of
  Nil -> False
  b :* v'
    | a == b    -> True
    | otherwise -> member v' a

-- }}}

-- Low Dim Proxies {{{

type Zero  = Z
type One   = S Zero
type Two   = S One
type Three = S Two
type Four  = S Three
type Five  = S Four
type Six   = S Five

pZero :: Proxy Zero
pZero = Proxy
pOne :: Proxy One
pOne = Proxy
pTwo :: Proxy Two
pTwo = Proxy
pThree :: Proxy Three
pThree = Proxy
pFour :: Proxy Four
pFour = Proxy
pFive :: Proxy Five
pFive = Proxy
pSix :: Proxy Six
pSix = Proxy

-- }}}

{-
-- Fin Member NEq VecBasis {{{

data Fin (n :: UN) (x :: UN) where
  FN :: Nat x => Fin x x
  FP :: (Nat n, Nat x) => Fin (S n) (S x) -> Fin (S n) x

class (Nat m, Nat n) => NLE (m :: UN) (n :: UN) where
  weaken :: Nat x => Fin m x -> Fin n x
instance NLE Z Z where
  weaken = id
instance NLE Z (S Z) where
  weaken = weaken1
instance NLE Z (S n) => NLE Z (S (S n)) where
  weaken = weaken1 . weaken
instance NLE m n => NLE (S m) (S n) where
  -- Fin (S m) x -> Fin (S n) x
  weaken n = case n of
    FN    -> raiseFin  $ weaken $ lowerFin n   -- n :: Fin (S m) (S m)
    FP n' -> weaken1 $ weaken $ lowerFin n' -- n :: Fin (S m) x, x < n

class (NLE x n, NLE r n) => Complement2 n x r | n x -> r, n r -> x where
  complement :: Fin n x -> Fin n r

instance Complement2 n Z n where
  complement = undefined

weaken1 :: (Nat n, Nat x) => Fin n x -> Fin (S n) x
weaken1 n = case n of
  FN    -> FP FN
  FP n' -> FP $ weaken1 n'

data VecBasis (n :: UN) = forall (x :: UN). VecBasis (Fin n x)

fin :: forall n x. Fin n x -> Int
fin n = case n of
  FN    -> nat (Proxy :: Proxy n)
  FP n' -> pred $ fin n'

fin0 :: Fin Zero Zero
fin0 = FN

fin1_1 :: Fin One One
fin1_1 = FN
fin1_0 :: Fin One Zero
fin1_0 = FP FN

fin2_2 :: Fin Two Two
fin2_2 = FN
fin2_1 :: Fin Two One
fin2_1 = FP FN
fin2_0 :: Fin Two Zero
fin2_0 = FP (FP FN)

basisIndex :: Nat n => VecBasis n -> Int
basisIndex (VecBasis n) = fin n

indexBasis :: Nat n => Vec n a -> VecBasis n -> a
indexBasis v (VecBasis n) = unsafeIndex (fin n) v 

-- }}}
-}

-- Old Basis {{{

_Basis :: forall n a. Nat n => Int -> Lens' (Vec n a) a
_Basis n
  | n < 0 || n >= nat p
  = error "Vec basis out of bounds"
  | otherwise
  = lens (unsafeIndex n) setV
  where
  p :: Proxy n
  p = Proxy
  setV :: Vec n a -> a -> Vec n a
  setV v b = update n (const b) v

safeBasis :: forall m n a. (Nat m, Nat n, LessThan m n) => Proxy m -> Lens' (Vec n a) a
safeBasis p = lens (unsafeIndex n) $ \v b -> update n (const b) v
  where
  n = nat p

-- }}}

-- Nat {{{

suc :: p n -> Proxy (S n)
suc _ = Proxy

prd :: p (S n) -> Proxy n
prd _ = Proxy

class Nat n where
  type Pred n :: UN
  nat :: (Num a, Enum a) => Proxy n -> a
  generate' :: Int -> (Int -> a) -> Vec n a
  -- finN0     :: Fin n Zero
  -- raiseFin    :: Nat x => Fin n x -> Fin (S n) (S x)
  -- lowerFin  :: Nat x => Fin (S n) (S x) -> Fin n x
  -- generateP :: Fin n x -> (VecBasis n -> a) -> Vec n a
  _List :: Prism' [a] (Vec n a)
  _Set  :: Ord a => Prism' (Set a) (Vec n a)

instance Nat Z where
  type Pred Z = Z
  nat _ = 0
  generate' _ _ = Nil
  -- finN0 = FN
  -- -- n :: Fin (S Z) (S x) -- can only ever be (Fin (S Z) (S Z))
  -- raiseFin n = case n of
  --   FN -> FN
  -- lowerFin n = case n of
  --   FN   -> FN
  --   FP _ -> error "Never happens"
  -- generateP _ _ = Nil
  _List = prism' (const []) $ \l -> case l of
    [] -> Just Nil
    _  -> Nothing
  _Set = prism' (const S.empty) $ \s -> if S.null s
    then Just Nil
    else Nothing

instance Nat n => Nat (S n) where
  type Pred (S n) = n
  nat _ = succ $ nat (Proxy :: Proxy n)
  generate' n f = f (n - nat p) :* generate' n f
    where
    p :: Proxy (S n)
    p = Proxy
  -- finN0 = weaken1 finN0
  -- raiseFin n = case n of
  --   FN -> FN
  --   FP n' -> FP $ raiseFin n'
  -- lowerFin n = case n of
  --   FN    -> FN
  --   FP n' -> FP $ lowerFin n'
  -- generateP n f = undefined
  _List = prism'
    (\(a :* v) -> a : review _List v)
    (\l -> case l of
      (a : l') -> (a :*) <$> preview _List l'
      _        -> Nothing)
  _Set = prism'
    (\(a :* v) -> S.insert a $ review _Set v)
    (\s -> case S.minView s of
      Just (a,s') -> (a :*) <$> preview _Set s'
      _           -> Nothing)

index :: Nat n => Int -> Vec n a -> Maybe a
index n v
  | n < 0 || n >= dim v = Nothing
  | otherwise = Just $ unsafeIndex n v

unsafeIndex :: Int -> Vec n a -> a
unsafeIndex n v = case v of
  Nil -> error "Index out of bounds"
  a :* v'
    | n == 0 -> a
    | otherwise -> unsafeIndex (pred n) v'

update :: Nat n => Int -> (a -> a) -> Vec n a -> Vec n a
update n f v = case v of
  Nil -> Nil
  a :* v'
    | n == 0 -> f a :* v'
    | otherwise -> a :* update (pred n) f v'

(**:) :: Vec n a -> a -> Vec (S n) a
v **: a = case v of
  Nil -> singleton a
  b :* v' -> b :* (v' **: a)

reverse :: Vec n a -> Vec n a
reverse vec = case vec of
  Nil -> Nil
  a :* v -> reverse v **: a

transpose :: forall m n a. (Nat m, Nat n) => Vec m (Vec n a) -> Vec n (Vec m a)
transpose v = case v of
  Nil           -> replicate Nil
  Nil :* _      -> Nil
  (_ :* _) :* _ -> fmap head v :* transpose (fmap tail v)

-- }}}

-- LessEq LessThan (Fin) {{{

class (Nat x, Nat y) => LessEq x y
instance (Add2 a x y) => LessEq x y

class (Nat x, Nat y) => LessThan x y
instance (Add2 (S a) x y) => LessThan x y

-- }}}

-- Add (append) {{{

class (Nat m, Nat n, Nat r)
  => Add2 m n r | m n -> r, m r -> n  where
  append :: Vec m a -> Vec n a -> Vec r a
instance Nat n => Add2 Z n n where
  append _ v = v
instance (Nat m, Nat n, Add2 m n r) => Add2 (S m) n (S r) where
  append (a :* v1) v2 = a :* append v1 v2

class (Add2 m n r, Add2 n m r) => Add m n r | m n -> r, m r -> n, n r -> m
instance (Add2 m n r, Add2 n m r) => Add m n r

add :: Add m n r => p m -> p n -> Proxy r
add _ _ = Proxy

sub :: Add m n r => p r -> p m -> Proxy n
sub _ _ = Proxy

-- }}}

-- Mul (flatten) {{{

class (Nat m, Nat n, Nat r)
  => Mul m n r | m n -> r, m r -> n where
  flatten :: Vec m (Vec n a) -> Vec r a

instance Nat n => Mul Z n Z where
  flatten _ = Nil

instance (Nat m, Nat n, Mul m n r', Add r' n r) => Mul (S m) n r where
  flatten (v :* vs) = append v $ flatten vs

-- }}}

-- Combination (combinations) {{{

class (Nat n, Nat k, Nat r)
  => Combination n k r | n k -> r where
  choose :: Vec n a -> Vec r (Vec k a)

instance Combination Z Z (S Z) where
  choose _ = Nil :* Nil

instance Nat k => Combination Z (S k) Z where
  choose _ = Nil

instance Nat n => Combination (S n) Z (S Z) where
  choose _ = Nil :* Nil

instance (Combination n k r1, Combination n (S k) r2, Add r1 r2 r)
  => Combination (S n) (S k) r where
  choose (a :* v) = append
    ((a :*) <$> choose v)
    $ choose v

-- }}}

-- Eq Ord Show Num {{{

instance Eq a => Eq (Vec n a) where
  Nil == Nil = True
  (a :* v1) == (b :* v2) = a == b && v1 == v2
  _ == _ = error "Never happens"

instance Ord a => Ord (Vec n a) where
  compare Nil Nil = EQ
  compare (a :* v1) (b :* v2) = compare a b `mappend` compare v1 v2
  compare _ _ = error "Never happens"

instance (Nat n, Show a) => Show (Vec n a) where
  showsPrec d v = showParen (d > 5)
    $ showString "fromList "
    . shows (toList v)

instance (Num a, Nat n) => Num (Vec n a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Fractional a, Nat n) => Fractional (Vec n a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

-- }}}

-- Functor Applicative Apply {{{

instance Functor (Vec n) where
  fmap f vec = case vec of
    Nil -> Nil
    (a :* v) -> f a :* fmap f v

instance Nat n => Applicative (Vec n) where
  pure = replicate
  fs <*> as = case (fs,as) of
    (Nil,Nil) -> Nil
    (f :* fs',a :* as') -> f a :* (fs' <*> as')
    _ -> error "Never happens"

instance Apply (Vec n) where
  fs <.> as = case (fs,as) of
    (Nil,Nil) -> Nil
    (f :* fs',a :* as') -> f a :* (fs' <.> as')
    _ -> error "Never happens"

-- }}}

-- Monad Bind {{{

instance Nat n => Monad (Vec n) where
  return = pure
  v >>= f = diagonal $ fmap f v

instance Nat n => Bind (Vec n) where
  v >>- f = diagonal $ fmap f v

diagonal :: Nat n => Vec n (Vec n a) -> Vec n a
diagonal vec = case vec of
  Nil -> Nil
  v1 :* vs -> head v1 :* diagonal (tail <$> vs)

-- }}}

-- Monoid Semigroup {{{

instance (Nat n, Monoid a) => Monoid (Vec n a) where
  mempty = replicate mempty
  mappend vec1 vec2 = case (vec1,vec2) of
    (Nil,Nil) -> Nil
    (a :* v1,b :* v2) -> mappend a b :* mappend v1 v2
    _ -> error "Never happens"

instance (Nat n, Semigroup a) => Semigroup (Vec n a) where
  vec1 <> vec2 = case (vec1,vec2) of
    (Nil,Nil) -> Nil
    (a :* v1,b :* v2) -> (a <> b) :* (v1 <> v2)
    _ -> error "Never happens"

-- }}}

-- Foldable Foldable1 {{{

instance Nat n => Foldable (Vec n) where
  foldMap f vec = case vec of
    Nil -> mempty
    a :* v -> f a `mappend` foldMap f v

instance Nat n => Foldable1 (Vec (S n)) where
  foldMap1 f (a :* v) = case v of
    Nil -> f a
    _ :* _ -> f a <> foldMap1 f v

-- }}}

-- Traversable Traversable1 {{{

instance Nat n => Traversable (Vec n) where
  traverse f vec = case vec of
    Nil -> pure Nil
    a :* v -> (:*) <$> f a <*> traverse f v

instance Nat n => Traversable1 (Vec (S n)) where
  traverse1 f (a :* v) = case v of
    Nil -> (:* Nil) <$> f a
    _ :* _ -> (:*) <$> f a <.> traverse1 f v

-- }}}

-- Distributive {{{

instance Nat n => Distributive (Vec n) where
  distribute f = generate $ \i -> unsafeIndex i <$> f

-- }}}

-- Representable {{{

instance Nat n => R.Representable (Vec n) where
  type Rep (Vec n) = Lin.E (Vec n)
  tabulate f = generate $ \i -> f $ Lin.E $ _Basis i
  {-# INLINE tabulate #-}
  index v (Lin.E l) = v ^. l
  {-# INLINE index #-}

-- }}}

-- Ixed {{{

type instance Index (Vec n a) = Lin.E (Vec n)
type instance IxValue (Vec n a) = a

instance Ixed (Vec n a) where
  ix = Lin.el
  {-# INLINE ix #-}

instance Nat n => FunctorWithIndex Int (Vec n)
instance Nat n => FoldableWithIndex Int (Vec n)
instance Nat n => TraversableWithIndex Int (Vec n) where
  itraverse (f :: Int -> a -> f b) vec = go 0 vec
    where
    go :: Int -> Vec m a -> f (Vec m b)
    go n v = case v of
      Nil     -> pure Nil
      a :* v' -> (:*) <$> f n a <*> go (succ n) v'

-- }}}

-- SafeCons SafeSnoc {{{

class SafeCons s s' t t' a b
  | s -> a, s' -> a
  , t -> b, t' -> b
  , s -> s', s' -> s
  , t -> t', t' -> t
  , s' b -> t
  , t' a -> s where
  _Cons_ :: Iso (a,s') (b,t') s t

(<||) :: SafeCons s s' s s' a a => a -> s' -> s
(<||) = cons_

cons_ :: SafeCons s s' s s' a a => a -> s' -> s
cons_ = curry (simply view _Cons_)

uncons_ :: SafeCons s s' s s' a a => s -> (a,s')
uncons_ = simply view $ from _Cons_

class SafeSnoc s s' t t' a b
  | s -> a, s' -> a
  , t -> b, t' -> b
  , s -> s', s' -> s
  , t -> t', t' -> t
  , s' b -> t
  , t' a -> s where
  _Snoc_ :: Iso (s',a) (t',b) s t

(||>) :: SafeSnoc s s' s s' a a => s' -> a -> s
(||>) = snoc_

snoc_ :: SafeSnoc s s' s s' a a => s' -> a -> s
snoc_ = curry (simply view _Snoc_)

unsnoc_ :: SafeSnoc s s' s s' a a => s -> (s',a)
unsnoc_ = simply view $ from _Snoc_

instance Nat n => SafeCons (Vec (S n) a) (Vec n a) (Vec (S n) b) (Vec n b) a b where
  _Cons_ = iso
    (uncurry (:*))
    (head &&& tail)

instance Nat n => SafeSnoc (Vec (S n) a) (Vec n a) (Vec (S n) b) (Vec n b) a b where
  _Snoc_ = iso
    (uncurry (**:))
    (init &&& last)

-- }}}

-- Each {{{

instance Nat n => Each (Vec n a) (Vec n b) a b where
  each = traverse
  {-# INLINE each #-}

-- }}}

-- linear {{{

instance Nat n => Lin.Additive (Vec n) where
  zero = pure 0

instance Nat n => Lin.Metric (Vec n) where
  x `dot` y = F.sum $ liftA2 (*) x y

instance (Nat n, Lin.Epsilon a) => Lin.Epsilon (Vec n a) where
  nearZero = Lin.nearZero . Lin.quadrance

instance Nat n => Lin.Trace (Vec n)

instance Nat n => Lin.Affine (Vec n) where
  type Diff (Vec n) = Vec n
  (.-.) = (Lin.^-^)
  {-# INLINE (.-.) #-}
  (.+^) = (Lin.^+^)
  {-# INLINE (.+^) #-}
  (.-^) = (Lin.^-^)
  {-# INLINE (.-^) #-}

-- }}}

-- vector-space {{{

instance (Nat n, VS.AdditiveGroup a) => VS.AdditiveGroup (Vec n a) where
  zeroV = pure VS.zeroV
  (^+^) = liftA2 (VS.^+^)
  negateV = fmap VS.negateV

instance (Nat n, VS.VectorSpace a) => VS.VectorSpace (Vec n a) where
  type Scalar (Vec n a) = VS.Scalar a
  s *^ v = (s VS.*^) <$> v

instance (Nat n, VS.InnerSpace a) => VS.InnerSpace (Vec n a) where
  v1 <.> v2 = VS.sumV $ liftA2 (VS.<.>) v1 v2

instance (Nat n, VS.AffineSpace a) => VS.AffineSpace (Vec n a) where
  type Diff (Vec n a) = Vec n (VS.Diff a)
  (.-.) = liftA2 (VS..-.)
  (.+^) = liftA2 (VS..+^)

{-
instance (Nat n, VS.HasBasis a) => VS.HasBasis (Vec n a) where
  type Basis (Vec n a) = (VecBasis n,VS.Basis a)
  basisValue (n,a) = update n (const $ VS.basisValue a) VS.zeroV
  decompose v = ifoldMap go v
    where
    go :: Int -> a -> [((VecBasis n, VS.Basis a), VS.Scalar a)]
    go i = map (\(b,s) -> ((i,b),s)) . VS.decompose
  decompose' v (vb,b) = VS.decompose' (unsafeIndex (basisDim vb) v) b
-}

instance VS.AdditiveGroup a => VS.HasCross2 (Vec Two a) where
  cross2 (a :* b :* _) = VS.negateV b *: a
  cross2 _ = error "Never happens"

instance Num a => VS.HasCross3 (Vec Three a) where
  (ax :* ay :* az :* _) `cross3` (bx :* by :* bz :* _) =
    x :* y *: z
    where
    x = ay * bz - az * by
    y = az * bx - ax * bz
    z = ax * by - ay * bx
  cross3 _ _ = error "Never happens"

-- }}}

