{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.DSL where

import Control.Applicative (Applicative(..))
import Control.Arrow ((***),(&&&))
import Language.Haskell.TH.Syntax (Name(..),OccName(..),NameFlavour(..),mkName)
import GHC.Exts (Constraint,IsString(..))
import Data.Word
import Data.Bifunctor (Bifunctor)
import qualified Data.Bifunctor as B
import Data.Bifoldable
import Data.Biapplicative hiding ((<<$>>))
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Monoid (Monoid(..))

-- D {{{

class
  ( PatD  d ~ PatE  (ExpD d)
  , TypeD d ~ TypeE (ExpD d)
  , TypeD d ~ TypeC (CxtT (TypeD d))
  ) => D (d :: *) where
  type TypeD d :: *
  type PatD  d :: *
  type ExpD  d :: *
  fun  :: Name   -> [PatD d] -> ExpD d -> d
  val  :: PatD d -> ExpD  d  -> d
  sigD :: Name   -> TypeD d  -> d

instance
  ( D d1, D d2
  ) => D (d1,d2) where
  type TypeD (d1,d2) = (TypeD d1,TypeD d2)
  type PatD  (d1,d2) = (PatD  d1,PatD  d2)
  type ExpD  (d1,d2) = (ExpD  d1,ExpD  d2)
  fun nm (unzip -> (p1,p2)) (e1,e2) = (fun nm p1 e1,fun nm p2 e2)
  val (p1,p2) (e1,e2) = (val  p1 e1,val  p2 e2)
  sigD nm     (t1,t2) = (sigD nm t1,sigD nm t2)

-- }}}

-- T {{{

class
  ( t ~ TypeC (CxtT t)
  , Lits (LitsT t) (LitT t)
  ) => T (t :: *) where
  type KindT t :: *
  type CxtT  t :: *
  type LitT  t :: *
  type LitsT t :: [* -> Constraint]
  type LitsT t = '[TLCommon]
  forallT :: [(Name,Maybe (KindT t))] -> [CxtT t] -> t -> t
  appT    :: t -> t -> t
  sigT    :: t -> KindT t -> t
  varT    :: Name -> t
  conT    :: Name -> t
  arrowT  :: t
  litT    :: LitT t -> t

instance
  ( T t1, T t2
  ) => T (t1,t2) where
  type KindT (t1,t2) = (KindT t1,KindT t2)
  type CxtT  (t1,t2) = (CxtT  t1,CxtT  t2)
  type LitT  (t1,t2) = (LitT  t1,LitT  t2)
  type LitsT (t1,t2) = '[FstC (Lits (LitsT t1)),SndC (Lits (LitsT t2))]
  forallT ns (unzip -> (c1,c2)) (t1,t2) =
    (forallT ns1 c1 t1 , forallT ns2 c2 t2)
    where
    ns1 = map (fmap $ fmap fst) ns
    ns2 = map (fmap $ fmap snd) ns
  appT (a1,a2) (b1,b2) = (appT a1 b1, appT a2 b2)
  sigT (t1,t2) (k1,k2) = (sigT t1 k1, sigT t2 k2)
  varT nm = (varT nm,varT nm)
  conT nm = (conT nm,conT nm)
  arrowT  = (arrowT,arrowT)
  litT (l1,l2) = (litT l1, litT l2)

-- }}}

-- K {{{

class K (k :: *) where
  forallK :: [Name] -> k -> k
  varK    :: Name -> k
  conK    :: Name -> k
  arrowK  :: k -> k -> k
  starK   :: k

instance
  ( K k1, K k2
  ) => K (k1,k2) where
  forallK ns (k1,k2) = (forallK ns k1, forallK ns k2)
  varK nm                = (varK nm, varK nm)
  conK nm                = (conK nm, conK nm)
  arrowK (a1,a2) (b1,b2) = (arrowK a1 b1,arrowK a2 b2)
  starK                  = (starK, starK)

-- }}}

-- C {{{

class
  ( c ~ CxtT (TypeC c)
  ) => C (c :: *) where
  type TypeC c :: *
  equalP :: TypeC c -> TypeC c -> c
  classP :: Name -> [TypeC c] -> c

instance
  ( C c1
  , C c2
  ) => C (c1,c2) where
  type TypeC (c1,c2) = (TypeC c1, TypeC c2)
  equalP (a1,a2) (b1,b2) = (equalP a1 b1, equalP a2 b2)
  classP nm ts = (classP nm (map fst ts),classP nm (map snd ts))

-- }}}

-- P {{{

class Lits (LitsP p) (LitP p) => P (p :: *) where
  type LitP  p :: *
  type LitsP p :: [* -> Constraint]
  type LitsP p =  '[LString,LNum]
  litP   :: LitP p -> p
  varP   :: Name   -> p
  conP   :: Name -> [p] -> p
  wildP  :: p

instance
  ( P p1, P p2
  ) => P (p1,p2) where
  type LitP  (p1,p2) = (LitP p1,LitP p2)
  type LitsP (p1,p2) = '[FstC (Lits (LitsP p1)),SndC (Lits (LitsP p2))]
  litP (l1,l2) = (litP l1, litP l2)
  varP nm    = (varP nm, varP nm)
  conP nm ps = (conP nm (map fst ps), conP nm (map snd ps))
  wildP      = (wildP, wildP)

-- }}}

-- E {{{

class
  ( TypeE e ~ TypeC (CxtT (TypeE e))
  , Lits (LitsE e) (LitE e)
  ) => E (e :: *) where
  type TypeE e :: *
  type PatE  e :: *
  type LitE  e :: *
  type LitsE e :: [* -> Constraint]
  type LitsE e = '[LString,LNum]
  varE  :: Name -> e
  conE  :: Name -> e
  litE  :: LitE e  -> e
  appE  :: e -> e -> e
  lam   :: [PatE e] -> e -> e
  case_ :: e -> [(PatE e,e)] -> e
  sigE  :: e -> TypeE e -> e

instance
  ( E e1, E e2
  ) => E (e1,e2) where
  type TypeE (e1,e2) = (TypeE e1,TypeE e2)
  type PatE  (e1,e2) = (PatE  e1,PatE  e2)
  type LitE  (e1,e2) = (LitE  e1,LitE  e2)
  type LitsE (e1,e2) = '[FstC (Lits (LitsE e1)),SndC (Lits (LitsE e2))]
  varE nm = (varE nm, varE nm)
  conE nm = (conE nm, conE nm)
  litE (l1,l2) = (litE l1, litE l2)
  appE (f1,f2) (x1,x2) = (appE f1 x1, appE f2 x2)
  lam  ps (e1,e2) = (lam (map fst ps) e1, lam (map snd ps) e2)
  case_ (e1,e2) ms = (case_ e1 m1, case_ e2 m2)
    where
    m1 = map (fst *** fst) ms
    m2 = map (snd *** snd) ms
  sigE  (e1,e2) (t1,t2) = (sigE e1 t1, sigE e2 t2)

-- }}}

-- L {{{

class L (a :: *) (l :: *) where
  lit :: a -> l

instance
  ( L a l1, L a l2
  ) => L a (l1,l2) where
  lit = lit &&& lit

class LString (l :: *) where
  charL     :: Char     -> l
  stringL   :: String   -> l

instance
  ( LString l1, LString l2
  ) => LString (l1,l2) where
  charL   = charL   &&& charL
  stringL = stringL &&& stringL

class LNum (l :: *) where
  integerL  :: Integer  -> l
  rationalL :: Rational -> l

instance
  ( LNum l1, LNum l2
  ) => LNum (l1,l2) where
  integerL  = integerL  &&& integerL
  rationalL = rationalL &&& rationalL

class LPrim (l :: *) where
  intPrimL    :: Integer  -> l
  wordPrimL   :: Integer  -> l
  floatPrimL  :: Rational -> l
  doublePrimL :: Rational -> l
  stringPrimL :: [Word8]  -> l

instance
  ( LPrim l1, LPrim l2
  ) => LPrim (l1,l2) where
  intPrimL    = intPrimL    &&& intPrimL
  wordPrimL   = wordPrimL   &&& wordPrimL
  floatPrimL  = floatPrimL  &&& floatPrimL
  doublePrimL = doublePrimL &&& doublePrimL
  stringPrimL = stringPrimL &&& stringPrimL

class TLCommon (tl :: *) where
  stringTL :: String  -> tl
  intTL    :: Integer -> tl

class    Lits (cs :: [* -> Constraint]) (l :: *)
instance Lits '[] l
instance (c l, Lits cs l) => Lits (c ': cs) l

type family Fst (p :: *) where
  Fst (a,b) = a

type family Snd (p :: *) where
  Snd (a,b) = b

class    c (Fst p) => FstC (c :: * -> Constraint) (p :: *)
instance c (Fst p) => FstC c p

class    c (Snd p) => SndC (c :: * -> Constraint) (p :: *)
instance c (Snd p) => SndC c p

-- }}}

-- Util {{{

instance IsString OccName where
  fromString = OccName

instance IsString Name where
  fromString = mkName

(<<$>>) :: Bifunctor p => (a -> b) -> p a a -> p b b
(<<$>>) f = bimap f f
infixl 4 <<$>>

dup :: Biapplicative p => a -> p a a
dup a = bipure a a

bidistribute ::
  ( Applicative f -- ^ can make a singleton
  , Monoid (f a)  -- ^ can combine 2 into 1
  , Monoid (f b)  -- ^ can combine 2 into 1
  , Foldable f    -- ^ can eliminate the structure
  ) => f (a,b) -> (f a,f b)
bidistribute = F.foldMap $ bimap pure pure

bicollect :: forall f a b g c d.
  ( Applicative f   -- ^ can make a singleton
  , Bifoldable g
  , Monoid (g (f c) (f d))
  , Monoid (f c)
  , Monoid (f d)
  , Foldable f      -- ^ can eliminate the structure
  , Biapplicative g
  ) => (a -> c) -> (b -> d) -> f (g a b) -> g (f c) (f d)
bicollect f g = F.foldMap $ bifoldMap
  (\a -> bipure (pure $ f a) mempty)
  (\b -> bipure mempty (pure $ g b))


-- }}}

-- Is* Family {{{

-- The 'Is*' family of types
-- provide entry points to types in the
-- cyclic structure.

type IsDec d =
  ( D d
  , T (TypeD d)
  , K (KindT (TypeD d))
  , C (CxtT  (TypeD d))
  , P (PatD d)
  , E (ExpD d)
  )

type IsType t =
  ( T t
  , C (CxtT t)
  , K (KindT t)
  )

type IsCxt c =
  ( C c
  , T (TypeC c)
  , K (KindT (CxtT c))
  )

type IsKind k = ( K k )

type IsPat p = ( P p )

type IsExp e =
  ( E e
  , P (PatE  e)
  , T (TypeE e)
  )

type family ExpWith (cs :: [* -> Constraint]) (e :: *) where
  ExpWith '[] e       = E e
  ExpWith (c ': cs) e = (c (LitE e), ExpWith cs e)

-- }}}

-- Examples {{{

t0 :: IsType t => t
t0 = forallT
  [ ( "a" , Nothing ) ]
  [ ]
  $ appT (conT "Maybe") (varT "a")

t1 :: IsType t => t
t1 = forallT
  [ ( "a" , Just $ varK "k" ) ]
  [ ]
  $ appT (conT "Foo") (varT "a")

t2 :: IsType t => t
t2 = forallT
  [ ( "a" , Just $ varK "k" ) ]
  [ classP "Show" [varT "a"] ]
  $ appT (conT "Foo") (varT "a")

e0 :: IsExp e => e
e0 = varE "foo"

e1 :: IsExp e => e
e1 = lam
  [ varP "a"
  ]
  $ varE "a"

e2 :: (IsExp e, LString (LitE e)) => e
e2 = appE (lam [varP "a"] (sigE (varE "a") (conT "Char"))) (litE (charL 'c'))

-- }}}

