{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Data.Pattern where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List (intersperse)
import GHC.Exts (Constraint)
import GHC.TypeLits

data Pat :: * -> * -> * -> * where
  Var  :: Pat r a (r -> a)
  Wild :: Pat r a       a
  Lit  :: Eq r => r -> Pat r a a
  Con  :: HasCons r => Sum_ (Prod_ Pat) (Fields r) a b -> Pat r a b

data Prod (f :: k -> *) (rs :: [k]) where
  Nil  :: Prod f '[]
  (:*) :: { lhead :: f r
          , ltail :: Prod f rs
          } -> Prod f (r ': rs)
infixr 4 :*

data Prod_ (f :: k -> * -> * -> *) (rs :: [k]) :: * -> * -> * where
  Nil_  :: Prod_ f '[] a a
  (:**) :: f r b c -> Prod_ f rs a b -> Prod_ f (r ': rs) a c
infixr 4 :**

(*:) :: Applicative f => r -> Prod f rs -> Prod f (r ': rs)
a *: as = pure a :* as
infixr 4 *:

(*:*) :: Applicative f => a -> b -> Prod f '[a,b]
a *:* b = pure a :* pure b :* Nil
infix 5 *:*

data Sum (f :: k -> *) (rs :: [k]) where
  L ::     f r  -> Sum f (r ': rs)
  R :: Sum f rs -> Sum f (r ': rs)

data Sum_ (f :: k -> * -> * -> *) (rs :: [k]) :: * -> * -> * where
  L_ :: f r a b -> Sum_ f (r ': rs) a b
  R_ :: Sum_ f rs a b -> Sum_ f (r ': rs) a b

newtype Id a = Id { runId :: a }

instance Functor Id where
  fmap f (Id a) = Id $ f a

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id $ f x

instance Monad Id where
  return     = Id
  Id a >>= f = f a

type Match arr = (arr ~ Kleisli Maybe)

matchSum :: Match arr
  => Sum_ (Prod_ Pat) rs a b
  -> Sum  (Prod  Id ) rs
  -> arr b a
matchSum p r = case (p,r) of
  (L_ ps,L rs) -> matchProd ps rs
  (R_ p',R r') -> matchSum p' r'
  _            -> zeroArrow

matchProd :: Match arr
  => Prod_ Pat rs a b
  -> Prod  Id  rs
  -> arr b a
matchProd ps rs = case (ps,rs) of
  (Nil_      ,Nil         ) -> returnA
  (p' :** ps',Id r' :* rs') -> match p' r' >>> matchProd ps' rs'
  _ -> error "unreachable"

match :: Match arr
  => Pat r a b
  -> r
  -> arr b a
match p r = case p of
  Var    -> undefined -- Just . ($ r)
  Wild   -> returnA
  Lit r'  | r == r' -> returnA
          | True    -> zeroArrow
  Con ps -> undefined -- matchSum ps $ deconstruct r

class HasCons (r :: *) where
  -- data Con r    :: * -> * -> *
  type Fields r :: [[*]]
  -- matchCon    :: Con r a b -> r -> b -> Maybe a
  deconstruct :: r -> Sum (Prod Id) (Fields r)

instance HasCons [r] where
  type Fields [r] = '[ '[] , '[r,[r]] ]
  -- data Con [r] a b where
  --   Null :: Con [r] a a
  --   Cons :: Pat  r  b c
  --        -> Pat [r] a b
  --        -> Con [r] a c
  deconstruct r = case r of
    []   -> L Nil
    a:as -> R $ L $ a *:* as
  -- matchCon c r = case (c,r) of
  --   (Null,[])         -> Just
  --   (Cons p1 p2,a:as) -> match p1 a >=> match p2 as
  --   _                 -> const Nothing

{-
instance HasCons (r,s) where
  data Con (r,s) a b where
    Pair :: Pat  r    b c
         -> Pat  s    a b
         -> Con (r,s) a c
  matchCon (Pair p1 p2) (r,s) = match p1 r >=> match p2 s
-}

{-
-- Con {{{

{-
data family Con r :: {- [Symbol] -> Symbol -> -} [*] -> *

data instance Con [a] {- defd used -} rs where
  Null :: Con [a] {- '["Null","Cons"] "Null" -} '[]
  Cons :: Con [a] {- '["Null","Cons"] "Cons" -} '[a,[a]]

instance Show (Con [a] rs) where
  show c = case c of
    Null -> "Null"
    Cons -> "Cons"
-}

{-
data instance Con (a,b) rs where
  Pair :: Con (a,b) '[a,b]
instance Show (Con (a,b) rs) where
  show Pair = "Pair"
-}

-- }}}

-- Member / Subset / SetEqual {{{

class Member (a :: k) (as :: [k]) where
  mem :: Mem a as

data Mem (a :: k) (as :: [k]) where
  Head :: Mem a (a ': as)
  Tail :: Member a as => Mem a (b ': as)

instance Member a (a ': as) where
  mem = Head

instance Member a as => Member a (b ': as) where
  mem = Tail

class Subset (as :: [k]) (bs :: [k]) where
  sub :: Sub as bs

data Sub (as :: [k]) (bs :: [k]) where
  Done :: Sub '[] bs
  (:+) :: Mem a bs -> Sub as bs -> Sub (a ': as) bs
infixr 4 :+

instance Subset '[] bs where
  sub = Done

instance (Member a bs, Subset as bs) => Subset (a ': as) bs where
  sub = mem :+ sub

class (Subset as bs, Subset bs as) => SetEqual (as :: [k]) (bs :: [k]) where
  setEq :: (Sub as bs, Sub bs as)

instance (Subset as bs, Subset bs as) => SetEqual as bs where
  setEq = (sub,sub)

-- }}}

-- Pat {{{

data Pat (f :: * -> *) :: [Symbol] -> * -> * -> * -> * where
  -- | (f r) gives us HOAS
  Var  :: Pat f '[] r a (f r -> a)
  LitP :: (Eq r, Show r)
       => r
       -> Pat  f cs r  a a
  Con  :: (Show (Con r c rs), IsCon r)
       => Con  r c  rs
       -> Pats f cs rs a b
       -> Pat  f cs r  a b
  Wild :: Pat  f cs r  a a

{-
instance Show (Pat f r a b) where
  showsPrec d p = case p of
    Var      -> showString "Var"
    LitP l   -> showParen (d > 10)
      $ showString "Lit "
      . shows l
    -- Con c ps -> showParen (d > 10)
    --   $ showsPrec 11 c
    --   . showChar ' '
    --   . showsPrec 11 ps
    Wild     -> showString "Wild"
-}

-- }}}

-- Pats {{{

data Pats (f :: * -> *) :: [Symbol] -> [*] -> * -> * -> * where
  Nil  :: Pats f '[] '[] a a
  (:>) :: Pat f c1 r b c -> Pats f c2 rs a b -> Pats f (c1 ++ c2) (r ': rs) a c
infixr 4 :>

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

{-
instance Show (Pats f rs a b) where
  showsPrec d ps = case ps of
    Nil      -> id
    p :> ps' -> showParen (d > 4)
      $ showsPrec 5 p
      . showString " :> "
      . showsPrec 4 ps'
-}

-- }}}

-- Match {{{

data Match (f :: * -> *) :: [Symbol] -> * -> * -> * where
  Match :: Pat f ps a (f b) c -> c -> Match f ps a b

-- }}}
-}

{-
m0 :: Match f a a
m0 = Match Var id
  -- id :: f a -> f a
-}

{-
m1 :: Match f (a,b) a
m1 = Match (Con Pair $ Var :> Var :> Nil) $ \a _ -> a

m2 :: Match f (a,b) b
m2 = Match (Con Pair $ Var :> Var :> Nil) $ \_ b -> b
-}

