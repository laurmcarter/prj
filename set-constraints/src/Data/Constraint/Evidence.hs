{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Constraint.Evidence where

import Data.Proxy
import GHC.Exts (Constraint)

-- Evidence is a data family so that users may define
-- structured proofs of a constraint

data family Evidence (c :: Constraint) :: *

-- Dict {{{

-- A Dict is a shapeless piece of evidence for a constraint.

data Dict (c :: Constraint) where
  Dict :: c => Dict c

data Dict1 (c :: k -> Constraint) (a :: k) where
  Dict1 :: c a => Dict1 c a

data Dict2 (c :: k -> l -> Constraint) (a :: k) (b :: l) where
  Dict2 :: c a b => Dict2 c a b

(<+>) :: Dict c -> Dict d -> Dict (c,d)
Dict <+> Dict = Dict
infixr 4 <+>

split :: Dict (c,d) -> (Dict c,Dict d)
split Dict = (Dict,Dict)

-- }}}

-- Implicit {{{

class Implicit p where
  implicit :: p

instance Implicit () where
  implicit = ()

instance (Implicit p, Implicit q) => Implicit (p,q) where
  implicit = (implicit,implicit)

instance c => Implicit (Dict c) where
  implicit = Dict

instance c a => Implicit (Dict1 c a) where
  implicit = Dict1

instance c a b => Implicit (Dict2 c a b) where
  implicit = Dict2

-- }}}

-- FromDict {{{

class FromDict c p | p -> c where
  fromDict :: Dict c -> p

instance FromDict c (Dict c) where
  fromDict Dict = Dict

instance FromDict (c a) (Dict1 c a) where
  fromDict Dict = Dict1

instance FromDict (c a b) (Dict2 c a b) where
  fromDict Dict = Dict2


class FromDict1 c a p | p -> c a where
  fromDict1 :: Dict1 c a -> p

instance FromDict1 c a (Dict (c a)) where
  fromDict1 Dict1 = Dict

instance FromDict1 c a (Dict1 c a) where
  fromDict1 Dict1 = Dict1

instance FromDict1 (c a) b (Dict2 c a b) where
  fromDict1 Dict1 = Dict2


class FromDict2 c a b p | p -> c a b where
  fromDict2 :: Dict2 c a b -> p

instance FromDict2 c a b (Dict (c a b)) where
  fromDict2 Dict2 = Dict

instance FromDict2 c a b (Dict1 (c a) b) where
  fromDict2 Dict2 = Dict1

instance FromDict2 c a b (Dict2 c a b) where
  fromDict2 Dict2 = Dict2

-- }}}

-- Witness {{{

class Witness w c | w -> c where
  witness :: w -> (Dict c -> r) -> r

instance Witness (Dict c) c where
  witness Dict f = f Dict

instance Witness (Dict1 c a) (c a) where
  witness Dict1 f = f Dict

instance Witness (Dict2 c a b) (c a b) where
  witness Dict2 f = f Dict

-- }}}

class    c b a => Flip c a b
instance c b a => Flip c a b

type Impl c = (c,Implicit (Evidence c))

