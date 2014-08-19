{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Data.Constraint.Set where

import Data.Constraint.Evidence
import GHC.Exts (Constraint)
import Data.Proxy
import Data.Type.Equality

-- Elem {{{

class Elem (a :: k) (as :: [k])
instance Elem a (a ': as)
instance Elem a as => Elem a (b ': as)

type ElemEv a as = Evidence (Elem a as)

data instance Evidence (Elem a as) where
  ElemZ  :: ElemEv a (a ': as)
  ElemS :: !(ElemEv a as) -> ElemEv a (b ': as)

instance Implicit (ElemEv a (a ': as)) where
  implicit = ElemZ

instance Implicit (ElemEv a as) => Implicit (ElemEv a (b ': as)) where
  implicit = ElemS implicit

-- }}}

-- All {{{

class All (c :: k -> Constraint) (as :: [k])
instance All c '[]
instance (c a, All c as) => All c (a ': as)

type AllEv c as = Evidence (All c as)

data instance Evidence (All c as) where
  AllZ :: AllEv c '[]
  AllS :: !(Dict1 c a) -> !(AllEv c as) -> AllEv c (a ': as)

instance Witness (AllEv c as,ElemEv b as) (c b) where
  witness (AllS Dict1 _ , ElemZ     ) f = f Dict
  witness (AllS _ aEv   , ElemS eEv ) f = witness (aEv,eEv) f
  witness (AllZ         , _         ) _ = error "ghc pattern match bug"

instance Implicit (Evidence (All c '[])) where
  implicit = AllZ

instance (c a, Implicit (Evidence (All c as))) => Implicit (Evidence (All c (a ': as))) where
  implicit = AllS implicit implicit

withElem :: forall c as b r.
  ( Impl (All c as) , Impl (Elem b as)
  ) => Proxy as -> (Dict1 c b -> r) -> r
withElem _ f = witness
  ( implicit ::
    ( AllEv c as
    , ElemEv b as
    )
  ) $ f . fromDict

instance Witness (AllEv c '[]) () where
  witness AllZ f = f Dict

instance Witness (AllEv c as)    rest
  => Witness (AllEv c (a ': as)) (c a, rest) where
  witness (AllS Dict1 ev) f = witness ev $ f . (Dict <+>)

-- }}}

-- Subset {{{

type Subset   as bs = All   (Flip Elem bs) as
type SubsetEv as bs = AllEv (Flip Elem bs) as

-- }}}

