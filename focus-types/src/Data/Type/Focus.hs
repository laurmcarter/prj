{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Type.Focus where

import Control.Lens hiding ((<|),(|>))
import Data.Type.Equality

-- | build an equality over applied types from two
-- smaller equalities, one of kind (* -> *), one of kind (*).
(@:) :: (f :~: g) -> (a :~: b) -> (f a :~: g b)
(@:) = apply
infixl 4 @:

(<@>) :: (a :~: c) -> (b :~: d) -> (f a b :~: f c d)
ac <@> bd = Refl @: ac @: bd

-- | apply an equality under a Setter
(>-) :: ASetter (c :~: c) r a b -> (a :~: b) -> r
l >- p = Refl & l %~ castWith p
infixr 2 >-

(=<|) :: x :~: y -> y :~: z -> x :~: z
(=<|) = trans
infixr 1 =<|

(|>) :: x :~: y -> p y -> x :~: y
p |> _ = p
infixl 2 |>

(>|) :: (w :~: x -> y :~: z) -> w :~: x -> y :~: z
(>|) = ($)
infixr 1 >|

(..|) :: (w :~: x -> y :~: z) -> w :~: x -> y :~: z
(..|) = ($)
infixr 1 ..|

(|..) :: (w :~: x -> y :~: z) -> p w -> w :~: x -> y :~: z
f |.. _ = f
infixl 2 |..

(|<) :: (w :~: x -> y :~: z) -> p w -> w :~: x -> y :~: z
f |< _ = f
infixl 2 |<

begin :: p x -> x :~: x
begin _ = Refl

byDef :: x :~: x
byDef = Refl

sub :: (w :~: x -> y :~: z) -> w :~: x -> y :~: z
sub f x = f x

wit :: p x -> x :~: x
wit _ = Refl

(==>) :: Maybe (a :~: b) -> ((a ~ b) => Maybe r) -> Maybe r
mp ==> r = case mp of
  Just Refl -> r
  _         -> Nothing
infixl 4 ==>

qed :: Maybe (a :~: a)
qed = Just Refl

(|-) :: (a :~: b) -> ((a ~ b) => r) -> r
Refl |- r = r
infixl 4 |-

