{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Pattern.Class where

import GHC.TypeLits

class Pat (p :: * -> * -> * -> *) where
  type ConOf p  :: Symbol -> * -> [*] -> *
  type LitOf p  ::  *  -> *
  type NameOf p :: Symbol -> *
  varP  :: p r a (r -> a)
  wildP :: p r a  a
  litP  :: Eq r => LitOf p r -> p r a a
  conP  :: ( ConPat p r as a bs b t
           ) => ConOf p x r as -> t -> p r a b

class ConPat (p :: * -> * -> * -> *)
  (r :: *) (as :: [*])
  (a :: *) (bs :: [*]) (b :: *) (t :: *)
  | p r as a bs b -> t
  , p r as a bs t -> b
  , p r as b bs t -> a
  , p r as a b  t -> bs
  , t -> bs
  , t bs -> b
  , p t -> r

instance
  ConPat p r '[]       a      '[]  a (p r a a)

instance
     ConPat p r       xs  a       bs  b             t
  => ConPat p r (x ': xs) a (b ': bs) c (p x b c -> t)

class Pats (ps :: [*] -> * -> * -> *) where
  type PatOf ps :: * -> * -> * -> *
  nil :: ps '[] a a
  (&) :: PatOf ps r b c -> ps rs a b -> ps (r ': rs) a c
infixr 4 &

-- IsCon {{{

class KnownSymbol x
  => IsCon (x :: Symbol) (r :: *) (as :: [*])
  | x r -> as

instance IsCon "Null" [r] '[]
instance IsCon "Cons" [r] '[r,[r]]
-- open definitions of constructors whaaa?
instance IsCon "Head" [r] '[r]
instance IsCon "Tail" [r] '[[r]]

instance IsCon "Pair" (r,s) '[r,s]

data Name (x :: Symbol) where
  Name :: KnownSymbol x => Name x

data Con :: Symbol -> * -> [*] -> * where
  Con :: IsCon x r as => Name x -> Con x r as

headCon :: Con "Head" [r] '[r]
headCon  = Con Name

tailCon :: Con "Tail" [r] '[[r]]
tailCon  = Con Name

nullCon :: Con "Null" [r] '[]
nullCon  = Con Name

consCon :: Con "Cons" [r] '[r,[r]]
consCon  = Con Name

pairCon :: Con "Pair" (r,s) '[r,s]
pairCon  = Con Name

-- }}}

