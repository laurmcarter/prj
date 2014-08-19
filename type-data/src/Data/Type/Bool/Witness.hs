{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Type.Bool.Witness where

import Data.Type.Bool
import Data.Type.Equality
import Data.Type.Known
import Control.Lens

data B (x :: Bool) where
  True_  :: B True
  False_ :: B False

instance Known B True where
  known = True_

instance Known B False where
  known = False_

instance TestEquality B where
  testEquality x y = case (x,y) of
    (True_ ,True_ ) -> Just Refl
    (True_ ,False_) -> Nothing
    (False_,True_ ) -> Nothing
    (False_,False_) -> Just Refl

class Decidable (t :: k -> *) where
  decide :: t a -> t b -> B (a == b)

instance Decidable B where
  decide a b = case (a,b) of
    (True_ ,True_ ) -> True_
    (False_,False_) -> True_
    (True_ ,False_) -> False_
    (False_,True_ ) -> False_

class Decide (a :: k) (b :: k) where
  _Decide :: Iso' (B (a == b)) (Maybe (a :~: b))

decideTrue :: Iso' (B True) (Maybe (a :~: a))
decideTrue = iso ( const $ Just Refl ) $ const True_

decideFalse :: Iso' (B False) (Maybe (a :~: b))
decideFalse = iso ( const Nothing ) $ const False_

instance Decide True  True  where _Decide = decideTrue
instance Decide True  False where _Decide = decideFalse
instance Decide False True  where _Decide = decideFalse
instance Decide False False where _Decide = decideTrue

bCase :: (x ~ True => r) -> (x ~ False => r) -> B x -> r
bCase t f x = case x of { True_  -> t ; False_ -> f }

ifTF :: B x -> (If x True False) :~: x
ifTF = bCase Refl Refl

ifAnd :: B x -> B y -> (If x y False) :~: (x && y)
ifAnd = const . bCase Refl Refl

ifOr :: B x -> B y -> (If x True y) :~: (x || y)
ifOr = const . bCase Refl Refl

ifBranch :: B x -> B y -> B z -> (If x y z) :~: (x && y || Not x && z)
ifBranch = const . const . bCase Refl Refl

