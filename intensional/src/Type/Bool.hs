{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Type.Bool where

import Type.Type
import Type.Value
import Type.Witness

data Boolean (b :: Bool) where
  BT :: Boolean True
  BF :: Boolean False

type IsBool b = (WitBy b Boolean,Reps b Bool)

-- Witness {{{

instance Witness False where
  type Wit False = Boolean
  witness = BF

instance Witness True where
  type Wit True = Boolean
  witness = BT

-- }}}

-- Value {{{

instance WitBy b Boolean => Value b where
  type Val b = Bool
  toVal b = case b of
    BF -> False
    BT -> True

-- }}}

-- Not {{{

type family Not (x :: Bool) :: Bool where
  Not False = True
  Not True  = False

neg :: Type x -> Type (Not x)
neg Type = Type

-- }}}

-- And {{{

type family And (x :: Bool)  (y :: Bool) :: Bool where
  And False y     = False
  And x     False = False
  And True  True  = True

type x :&&  y = And x y
infixr 5 :&&

conj :: Type x -> Type y -> Type (x :&& y)
conj Type Type = Type

-- }}}

-- Or {{{

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or False False = False
  Or x     True  = True
  Or True  y     = True

type x :||  y = Or  x y
infixr 4 :||

disj :: Type x -> Type y -> Type (x :|| y)
disj Type Type = Type

-- }}}

-- Xor {{{

type family Xor (x :: Bool) (y :: Bool) :: Bool where
  Xor False False = False
  Xor False True  = True
  Xor True  False = True
  Xor True  True  = False

type x :^^  y = Xor x y
infixr 5 :^^

xor :: Type x -> Type y -> Type (x :^^ y)
xor Type Type = Type

-- }}}

-- Implication {{{

type x :->  y =   Not x :|| y
infixr 7 :->

-- }}}

-- Iff {{{

type x :<-> y = x :-> y :&& y :-> x
infixr 6 :<->

-- }}}

