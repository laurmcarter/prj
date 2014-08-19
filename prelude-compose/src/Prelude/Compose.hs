{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Prelude.Compose where

import Prelude hiding (id,(.))
import qualified Prelude
import Control.Applicative hiding ((<**>))
import Data.Function (on)
import Data.Monoid
import Data.Proxy
import Data.Type.Equality
import Data.Functor.Contravariant
import GHC.Exts (Constraint)

class Compose f g r | f g -> r where
  comp :: f -> g -> r

instance Compose (b -> c) (a -> b) (a -> c) where
  comp = (.)

instance Compose f g r => Compose f (a -> g) (a -> r) where
  comp f g = comp f . g

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = comp

(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:.) = comp


type family FromVals (as :: [*]) (bs :: [*]) :: * where
  FromVals (a ': as) bs        = a -> FromVals as bs
  FromVals '[]       (b ': bs) = (b,FromVals '[] bs)
  FromVals '[]       '[]       = ()

fromVals :: Vals as bs -> FromVals as bs
fromVals vs = case vs of
  A f     -> fromVals . f
  U       -> ()
  b :* bs -> (b,fromVals bs)

class ToVals (t :: *) (as :: [*]) (bs :: [*]) | t -> as bs where
  toVals :: t -> Vals as bs

class ToVals' (p :: Maybe Typ) (t :: *) (as :: [*]) (bs :: [*]) | p t -> as bs where
  toVals' :: prx p -> t -> Vals as bs

instance (TCase t p, ToVals' p t as bs) => ToVals t as bs where
  toVals = toVals' (Proxy :: Proxy p)

instance ToVals' (Just Un) () '[] '[] where
  toVals' _ _ = U

instance ToVals t '[] bs => ToVals' (Just Tu) (b,t) '[] (b ': bs) where
  toVals' _ (b,t) = b :* toVals t

instance ToVals t as bs => ToVals' (Just Ar) (a -> t) (a ': as) bs where
  toVals' _ f = A $ toVals . f
  
instance ToVals' Nothing a '[] '[a] where
  toVals' _ = (:* U)


data Vals (as :: [*]) (rs :: [*]) where
  A     :: (a -> Vals as rs)  -> Vals (a ': as) rs
  U     ::                       Vals '[] '[]
  (:*) :: r  -> Vals '[] rs  -> Vals '[] (r ': rs)
infixr 4 :*

class Prefixed (as :: [k]) (bs :: [k]) (cs :: [k])
  | as bs -> cs where

instance Prefixed as '[] as
instance Prefixed as  bs cs => Prefixed (x ': as) (x ': bs) cs

(.*) :: r -> Vals as rs -> Vals as (r ': rs)
r .* as = case as of
  A f       -> A $ \x -> r .* f x
  U         -> r :* U
  r1 :* rs -> r :* r1 :* rs

cnst :: Vals as bs -> Vals (a ': as) bs
cnst = A . const

str :: Vals as bs -> Vals (a ': as) (a ': bs)
str v = case v of
  A f      -> A $ \y -> A $ (y .*) . f
  U        -> A (:* U)
  r :* rs -> A (:* r :* rs)

(@:) :: Vals (a ': as) bs -> a -> Vals as bs
A f @: x = f x
infixl 2 @:

(>@>) :: Vals as bs -> Vals bs cs -> Vals as cs
f >@> g = case f of
  A f'    -> A $ (>@> g) . f'
  U       -> g
  a :* as -> as >@> g @: a
infixr 1 >@>

(<@<) :: Vals bs cs -> Vals as bs -> Vals as cs
(<@<) = flip (>@>)
infixr 1 <@<

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)


upProdL :: Vals ((a,b) ': as) bs -> Vals (b ': a ': as) bs
upProdL vs = case vs of
  A f -> A $ \b -> A $ \a -> f (a,b)

upProdR :: Vals as ((a,b) ': bs) -> Vals as (a ': b ': bs)
upProdR vs = case vs of
  A f         -> A $ upProdR . f
  (a,b) :* rs -> a :* b :* rs

downProdL :: Vals (b ': a ': as) bs -> Vals ((a,b) ': as) bs
downProdL vs = A $ \(a,b) -> vs @: b @: a

downProdR :: Vals as (a ': b ': bs) -> Vals as ((a,b) ': bs)
downProdR vs = case vs of
  A f          -> A $ downProdR . f
  a :* b :* bs -> (a,b) :* bs
  _            -> noReally


class SubCategory (cat :: k -> k -> *) where
  type Id   cat (a :: k)                   :: Constraint
  type Comp cat (a :: k) (b :: k) (c :: k) :: Constraint
  id  :: Id cat a => cat a a
  (.) :: Comp cat a b c => cat b c -> cat a b -> cat a c

instance SubCategory (->) where
  type Id   (->) a     = ()
  type Comp (->) a b c = ()
  id  = Prelude.id
  (.) = (Prelude..)

instance SubCategory Vals where
  type Id   Vals as       = KnownArity as
  type Comp Vals as bs cs = ()
  id  = id_
  (.) = (<@<)

class SubProfunctor (p :: k -> k -> *) (q :: l -> l -> *) where
  type LMap p q (la :: k) (lb :: k) (a :: l) (b :: l) :: Constraint
  type RMap p q (la :: k) (lb :: k) (a :: l) (b :: l) :: Constraint
  lmap :: LMap p q la lb a b => q a b -> p lb lc -> p la lc
  rmap :: RMap p q la lb a b => q a b -> p lc la -> p lc lb
  dimap :: (LMap p q la lb a b, RMap p q lc ld c d)
    => q a b -> q c d -> p lb lc -> p la ld
  dimap f g = rmap g . lmap f

instance SubProfunctor (->) (->) where
  type LMap (->) (->) la lb a b = (la ~ a, lb ~ b)
  type RMap (->) (->) la lb a b = (la ~ a, lb ~ b)
  lmap = flip (Prelude..)
  rmap =      (Prelude..)

instance SubProfunctor (->) Vals where
  type LMap (->) Vals a b as bs = (as ~ '[a],bs ~ '[b])
  type RMap (->) Vals a b as bs = (as ~ '[a],bs ~ '[b])
  lmap g f a = case g @: a of
    b :* U -> f b
    _      -> noReally
  rmap g f c = case g @: f c of
    b :* U -> b
    _      -> noReally

instance SubProfunctor Vals (->) where
  type LMap Vals (->) as bs a b = ContraVals as bs a b
  type RMap Vals (->) as bs a b =    MapVals as bs a b
  lmap = cmapV
  rmap = fmapV

instance SubProfunctor Vals Vals where
  type LMap Vals Vals as bs cs ds = (as ~ cs, bs ~ ds)
  type RMap Vals Vals as bs cs ds = (as ~ cs, bs ~ ds)
  lmap = (>@>)
  rmap = (<@<)


v0 :: Vals '[] '[Int,Double,Bool]
v0 = 3 :* 4.2 :* True :* U

v1 :: Vals '[Int,Double,Bool] '[]
v1 = A $ \_ -> A $ \_ -> A $ \_ -> U

v1b :: Vals '[Int,Double,Bool] '[]
v1b = cnst $ cnst $ cnst U

v2 :: Vals '[Int,Double,Bool] '[Int,Double,Bool]
v2 = A $ \x -> A $ \y -> A $ \z -> x :* y :* z :* U

v2b :: Vals '[Int,Double,Bool] '[Int,Double,Bool]
v2b = str $ str $ str $ U

type family AllEq (a :: k) (as :: [k]) :: Constraint where
  AllEq a '[]       = ()
  AllEq a (b ': as) = (a ~ b, AllEq a as)

class KnownArity (as :: [*]) where
  id_    :: Vals as as
  const_ :: Vals '[] bs -> Vals as bs
  rep_   :: AllEq a as => a -> Vals '[] as

instance KnownArity '[] where
  id_    = U
  const_ = id
  rep_ _ = U

instance KnownArity as => KnownArity (a ': as) where
  id_    = str id_
  const_ = cnst . const_
  rep_ a = a :* rep_ a

type family Rep (as :: [*]) (x :: *) :: [*] where
  Rep '[]       x = '[]
  Rep (a ': as) x = x ': Rep as x

class MapVals as bs a b
  | as b -> bs, bs a -> as where
  fmapV :: (a -> b) -> Vals xs as -> Vals xs bs

instance MapVals '[] '[] a b where
  fmapV _ = id

instance MapVals as bs a b => MapVals (a ': as) (b ': bs) a b where
  fmapV f vs = case vs of
    A g      -> A $ fmapV f . g
    a :* as -> f a :* fmapV f as


class ContraVals as bs a b
  | as b -> bs, bs a -> as where
  cmapV :: (a -> b) -> Vals bs xs -> Vals as xs

instance ContraVals '[] '[] a b where
  cmapV _ = id

instance ContraVals as bs a b => ContraVals (a ': as) (b ': bs) a b where
  cmapV f (A g) = A $ cmapV f . g . f

data Dict (c :: Constraint) where
  Dict :: c => Dict c

noReally :: a
noReally = error "should be unreachable"

data Foo = Foo
  { is :: [Int]
  , bs :: [Bool]
  }

{-
instance Monoid Foo where
  mempty  = Foo mempty mempty
  mappend = 
-}

data Typ
  = Un
  | Tu
  | Ar

class    TCase (t :: *) (b :: Maybe Typ) | t -> b
instance b ~ Just Un => TCase ()       b
instance b ~ Just Tu => TCase (r,s)    b
instance b ~ Just Ar => TCase (r -> s) b
instance b ~ Nothing => TCase a        b

