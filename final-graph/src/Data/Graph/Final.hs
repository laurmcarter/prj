{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Data.Graph.Final where

class Graph (r :: [N] -> [N] -> [N] -> [N] -> * -> *) where
  type NLab r :: *
  type ELab r :: *
  node :: Fresh i ni no => NLab r -> r ni no e e (Node i)
  edge :: (Fresh i ei eo, Member x n, Member y n) => (NLab r -> NLab r -> ELab r)
       -> Node x -> Node y -> r n n ei eo (Edge i)
  bind :: r ni n ei e a -> (a -> r n no e eo b) -> r ni no ei eo b

type Fresh i li lo = (Next li i, Insert i li lo, Member i lo)

class Next (ns :: [N]) (r :: N) | ns -> r where
  next :: List a ns -> Nat r

instance Next '[] Z where
  next Nil = Z_

instance Next1 n ns r => Next (n ': ns) r where
  next ((n,_) :* ns) = next1 n ns

class Next1 (n :: N) (ns :: [N]) (r :: N) | n ns -> r where
  next1 :: Nat n -> List a ns -> Nat r

instance Next1 n '[] (S n) where
  next1 n Nil = S_ n

instance Next1 (S n) ns r => Next1 n (S n ': ns) r where
  next1 n ((sn,_) :* ns) = next1 sn ns

instance Next1 n (m ': ns) (S n) where
  next1 n (_ :* ns) = S_ n

class Insert (n :: N) (ns :: [N]) (r :: [N]) | n ns -> r, ns r -> n where
  insert :: Nat n -> a -> List a ns -> List a r

instance Insert n '[] '[n] where
  insert n a Nil = (n,a) :* Nil

instance Insert (S n) (n ': ns) (n ': S n ': ns) where
  insert sn a (n :* ns) = n :* (sn,a) :* ns

instance Insert n ns r => Insert n (m ': ns) (m ': r) where
  insert n a (m :* ns) = m :* insert n a ns

fresh :: Fresh i li lo => a -> List a li -> (Nat i,List a lo)
fresh a li = (i,insert i a li)
  where
  i = next li

data Mem_ (n :: N) (ns :: [N]) where
  Head :: Mem_ n (n ': ns)
  Tail :: Mem_ n ns -> Mem_ n (m ': ns)

class Member (n :: N) (ns :: [N]) where
  evid :: Mem_ n ns

instance Member n (n ': ns) where
  evid = Head

instance Member n ns => Member n (m ': ns) where
  evid = Tail evid

lookp :: forall n ns a. Member n ns => Nat n -> List a ns -> a
lookp n = go (evid :: Mem_ n ns)
  where
  go :: forall x xs. Mem_ x xs -> List a xs -> a
  go ev ns = case (ev,ns) of
    (Head    ,(_,a):*_  ) -> a
    (Tail ev',    _:*ns') -> go ev' ns'
    _ -> error "totality bug in GHC"

-- Nat Types {{{

newtype Node (n :: N) =  Node { getNode :: Nat n }
newtype Edge (n :: N) =  Edge { getEdge :: Nat n }

data N
  = Z
  | S N
  deriving (Eq,Show)

natVal :: Nat n -> Int
natVal n = case n of
  Z_    -> 0
  S_ n' -> 1 + natVal n'

data Nat (n :: N) where
  Z_ :: Nat Z
  S_ :: Nat n -> Nat (S n)

class KnownNat (n :: N) where
  nat :: Nat n

instance KnownNat Z where
  nat = Z_

instance KnownNat n => KnownNat (S n) where
  nat = S_ nat

data List a (ns :: [N]) where
  Nil  :: List a '[]
  (:*) :: (Nat n,a) -> List a ns -> List a (n ': ns)
infixr 4 :*

natEq :: Nat m -> Nat n -> Maybe (EQ m n)
natEq m n = case (m,n) of
  (Z_   ,Z_   ) -> Just EqZ
  (Z_   ,S_ n') -> Nothing
  (S_ m',Z_   ) -> Nothing
  (S_ m',S_ n') -> fmap EqS $ natEq m' n'

data EQ (a :: N) (b :: N) where
  EqZ :: EQ Z Z
  EqS :: EQ m n -> EQ (S m) (S n)

withEq :: EQ m n -> (m ~ n => r) -> r
withEq eq r = case eq of
  EqS eq' -> withEq eq' r
  EqZ     -> r

-- }}}

newtype E n e (ni :: [N]) (no :: [N]) (ei :: [N]) (eo :: [N]) a = E
  { eval_ :: (Nodes n ni,Edges e ei) -> (a,(Nodes n no,Edges n e eo))
  }

-- data EState ni no ei eo where
  

instance Graph (E n e) where
  type NLab (E n e) = n
  type ELab (E n e) = e
  node n = E $ \(ni,e) ->
    let (i,no) = fresh n ni
    in (Node i,(no,e))
  edge f (Node x) (Node y) = E $ \(n,ei) ->
    let xl                = lookp x n
        yl                = lookp y n
        (i,eo) = fresh (f xl yl) ei
    in (Edge i,(n,eo))
  bind m f = E $ \p0 ->
    let (a,p1) = eval_ m p0
    in eval_ (f a) p1

eval :: E n e '[] no '[] eo a -> (a,(List n no,List e eo))
eval e = eval_ e (Nil,Nil)

data Dir
  = ToLeft
  | ToRight
  deriving (Eq,Show)

(<--) :: (Graph r, ELab r ~ Dir, Fresh i ei eo, Member x n, Member y n)
  => Node x -> Node y -> r n n ei eo (Edge i)
(<--) = edge $ \_ _ -> ToLeft
infixr 4 <--

(-->) :: (Graph r, ELab r ~ Dir, Fresh i ei eo, Member x n, Member y n)
  => Node x -> Node y -> r n n ei eo (Edge i)
(-->) = edge $ \_ _ -> ToRight
infixr 4 -->

g0 () =
  bind (node "foo") $ \n1 ->
  bind (node "bar") $ \n2 ->
  n1 --> n2


