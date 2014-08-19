{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test where

import Data.Type.Focus
import Data.Type.Focus.TH

import Prelude hiding ((*),(+),pred,succ)
import Control.Applicative
import Data.Proxy
import qualified Data.Type.Bool     as Bool
import qualified Data.Type.Coercion as Coerce
import Data.Type.Equality
import qualified GHC.TypeLits as TL
import Unsafe.Coerce (unsafeCoerce)
import Data.Void
import GHC.Exts (Any)

-- Bool {{{

makePromotedEquality ''Bool

makeTyFamFoci
  [ ( ''Bool.Not  , "Not"   , "_not" )
  , ( ''(Bool.&&) , "And"   , "&&:"  )
  , ( ''(Bool.||) , "Or"    , "||:"  )
  , ( ''Bool.If   , "If"    , "_if"  )
  , ( ''(==)      , "DecEq" , "==:"  )
  ]

data B (x :: Bool) where
  True_  :: B True
  False_ :: B False

-- }}}

-- TL.Nat {{{

data Peano (x :: TL.Nat) where
  PZ :: Peano 0
  PS :: (x ~ Succ y) => Peano y -> Peano x

_PZ :: 0 :~: 0
_PZ = Refl

_PS :: x :~: y -> Succ x :~: Succ y
_PS Refl = Refl

instance TestEquality Peano where
  testEquality x y = case (x,y) of
    (PZ   ,PZ   ) -> qed
    (PS x',PS y') -> testEquality x' y' ==> qed
    _             -> Nothing

instance Coerce.TestCoercion Peano where
  testCoercion x y = Coerce.repr <$> testEquality x y

noSuchNat :: Peano (x :: TL.Nat)
noSuchNat = error "No such Nat!"

{-
exFalso :: a -> Void
exFalso _ = error "absurd case"
-}

-- }}}

-- Succ Pred {{{

type family Succ (x :: TL.Nat) :: TL.Nat where
  Succ x = 1 :+ x

succ :: Peano x -> Peano (Succ x)
succ = PS

_Succ :: (x :~: y) -> (Succ x :~: Succ y)
_Succ Refl = Refl

type family Pred (x :: TL.Nat) :: TL.Nat where
  Pred 0 = Any
  Pred 1 = 0
  Pred x = x :- 1

pred :: Peano x -> Peano (Pred x)
pred x = case x of
  PZ    -> noSuchNat
  PS x' -> unsafeCoerce x'

_Pred :: (x :~: y) -> (Pred x :~: Pred y)
_Pred Refl = Refl

exFalsoEq :: (1 :~: 0) -> Void
exFalsoEq = error "absurd == case"

exFalsoLe :: NatLe 1 0 -> Void
exFalsoLe = error "absurd <= case"

-- }}}

-- + - * {{{

type x :+ y = x TL.+ y
type x :- y = x TL.- y
type x :* y = x TL.* y

(+.) :: Peano x -> Peano y -> Peano (x :+ y)
x +. y = case x of
  PZ    -> y
  PS x' -> pDefAddS x' y |- PS (x' +. y)
infixl 6 +.

(.+.) :: (a :~: c) -> (b :~: d) -> (a :+ b) :~: (c :+ d)
Refl .+. Refl = Refl
infixl 6 .+.

(*.) :: Peano x -> Peano y -> Peano (x :* y)
x *. y = case x of
  PZ    -> PZ
  PS x' -> pDefMulS x' y |- (y +. (x' *. y))
infixl 7 *.

(.*.) :: (a :~: c) -> (b :~: d) -> (a :* b) :~: (c :* d)
Refl .*. Refl = Refl
infixl 7 .*.

data NatLe (x :: TL.Nat) (y :: TL.Nat) where
  LeZ :: TL.KnownNat y => NatLe 0 y
  LeS :: (x <= y)      => NatLe x y -> NatLe (Succ x) (Succ y)

class (x :: TL.Nat) <= (y :: TL.Nat) where
  natLe :: p0 x -> p1 y -> NatLe x y

instance TL.KnownNat y => (0 <= y) where
  natLe _ _ = LeZ 

instance (x <= y, x' ~ Succ x, y' ~ Succ y) => x' <= y' where
  natLe _ _ = LeS $ natLe (Proxy :: Proxy x) (Proxy :: Proxy y)

{-
(-.) :: (y <= x) => Peano x -> Peano y -> Peano (x :- y)
x -. y = case (x,y) of
  (_    ,PZ   ) -> x
  (PS x',PS y') -> case natLe y x of
    LeZ   -> _
    LeS _ -> _
    -- pDefSubS x' y' |- (x' -. y')
  (PZ   ,PS _ ) -> noSuchNat
infixl 7 -.
-}

(.-.) :: (a :~: c) -> (b :~: d) -> (a :- b) :~: (c :- d)
Refl .-. Refl = Refl
infixl 7 .-.

-- }}}

-- Unsafe Axioms {{{

pDefAddZ :: (0 :+ x) :~: x
pDefAddZ = Refl

pDefAddS :: Peano x -> Peano y -> (Succ x :+ y) :~: Succ (x :+ y)
pDefAddS _ _ = unsafeCoerce Refl

pDefMulZ :: (0 :* x) :~: 0
pDefMulZ = Refl

pDefMulS :: Peano x -> Peano y -> (Succ x :* y) :~: (y :+ (x :* y))
pDefMulS _ _ = unsafeCoerce Refl

pDefSubZ :: (x :- 0) :~: x
pDefSubZ = Refl

pDefSubS :: Peano x -> Peano y -> (Succ x :- Succ y) :~: (x :- y)
pDefSubS _ _ = unsafeCoerce Refl

pDefSuccPred :: Peano x -> Pred (Succ x) :~: x
pDefSuccPred _ = unsafeCoerce Refl

pDefSuccPred' :: Peano x -> Peano y -> (x :~: Succ y) -> (Pred x :~: y)
pDefSuccPred' x y p = begin (pred x)
  =<|     (sub _Pred |.. x
  ..|              p |>       succ y)
                     |> pred (succ y)
  =<| pDefSuccPred y |> y

pPosPredSucc :: (1 <= x) => Peano x -> Succ (Pred x) :~: x
pPosPredSucc x = case x of
  PS x' -> begin (succ (pred (succ x')))
    =<| (sub _Succ      |.. pred (succ x')
    ..| pDefSuccPred x' |>  x')
                        |>  succ x'
  PZ    -> error "unreachable"

pDefZNotS :: forall x. Peano x
  -> Either (x :~: 0   , NatLe 1 x -> Void)
            (NatLe 1 x , (x :~: 0) -> Void)
pDefZNotS x = case x of
  PZ               -> Left  (Refl,exFalsoLe)
  PS x' -> case pDefZNotS x' of
    Left  (Refl,_) -> Right (LeS LeZ,exFalsoEq)
    Right (le  ,_) -> _

-- }}}

-- TL Succ Pred Proofs {{{

-- }}}

-- TL Addition Proofs {{{

pAddZ :: Peano x -> (x :+ 0) :~: x
pAddZ _ = Refl

pAddS :: Peano x -> Peano y -> Succ (x :+ y) :~: (x :+ Succ y)
pAddS x y = case x of
  PZ    -> Refl
  PS x' -> begin                 (PS (PS  x' +. y))
    =<| (sub _PS                 |.. (PS  x' +. y)
    ..| pDefAddS x' y            |>   PS (x' +. y)
    =<| pAddS x' y               |>      (x' +. PS y))
                                 |>   PS (x' +. PS y)
    =<| sym (pDefAddS x' (PS y)) |>  (PS  x' +. PS y)

pAddComm :: Peano x -> Peano y -> (x :+ y) :~: (y :+ x)
pAddComm x y = case x of
  PZ    -> begin (PZ +. y)
         =<| pDefAddZ |> y
  PS x' -> begin         (PS  x' +.    y)
    =<| pDefAddS x' y |>  PS (x' +.    y)
    =<| (sub _PS      |..    (x' +.    y)
    ..| pAddComm x' y |>     (y  +.    x'))
                      |>  PS (y  +.    x')
    =<| pAddS y x'    |>     (y  +. PS x')

pAddAssoc :: Peano x -> Peano y -> Peano z -> (x :+ (y :+ z)) :~: ((x :+ y) :+ z)
pAddAssoc x y z = case x of
  PZ    -> begin                      (PZ +. y +. z)
    =<| pDefAddZ                   |>  (y  +. z)
    =<| (sub (.+. wit z)           |..  y
    ..| sym pDefAddZ               |>  (PZ +. y))
                                   |> ((PZ +. y) +. z)
  PS x' -> begin                       (PS   x' +. (y  +. z))
    =<| pDefAddS x' (y +. z)       |>   PS  (x' +. (y  +. z))
    =<| (sub _PS                   |..      (x' +. (y  +. z))
    ..| pAddAssoc x' y z           |>      ((x' +.  y) +. z))
                                   |>   PS ((x' +.  y) +. z)
    =<| sym (pDefAddS (x' +. y) z) |>  (PS  (x' +.  y) +. z)
    =<| (sub (.+. wit z)           |..  PS  (x' +.  y)
    ..| sym (pDefAddS x' y)        |>  (PS   x' +.  y))
                                   |> ((PS   x' +.  y) +. z)

pAddSwap :: Peano x -> Peano y -> Peano z -> (x :+ (y :+ z)) :~: (z :+ (y :+ x))
pAddSwap x y z = case x of
  PZ    -> begin                 (y +. z)
    =<| pAddComm y z |>          (z +.  y)
    =<| (sub (wit z .+.)     |..        y
    ..| sym (pAddZ y)        |>        (y +. PZ))
                             |>  (z +. (y +. PZ))
  PS x' -> begin                 (PS  x' +. (y +. z))
    =<| pDefAddS x' (y +. z) |>   PS (x' +. (y +. z))
    =<| (sub _PS             |..     (x' +. (y +. z))
    ..| pAddSwap x' y z      |>       (z  +. (y +. x')))
                             |>   PS (z  +. (y +. x'))
    =<| pAddS z (y +. x')    |>      (z +. PS (y +. x'))
    =<| (sub (wit z .+.)     |..           PS (y +. x')
    ..| pAddS y x'           |>           y +. PS x')
                             |>     z +. (y +. PS x')

-- }}}



-- Nat {{{

data N
  = Z
  | S N
  deriving (Eq,Show)

makePromotedEquality ''N

type instance a == b = EqNat a b
type family EqNat (x :: N) (y :: N) :: Bool where
  EqNat  Z     Z    = True
  EqNat  Z    (S y) = False
  EqNat (S x)  Z    = False
  EqNat (S x) (S y) = EqNat x y

data Nat (x :: N) where
  Z_ :: Nat Z
  S_ :: Nat x -> Nat (S x)

instance TestEquality Nat where
  testEquality x y = case (x,y) of
    (Z_,Z_)       -> qed
    (S_ x',S_ y') -> testEquality x' y' ==> qed
    _             -> Nothing

instance Coerce.TestCoercion Nat where
  testCoercion x y = Coerce.repr <$> testEquality x y

-- }}}

-- Nat Ops {{{

type family (x :: N) + (y :: N) :: N where
  Z   + y = y
  S x + y = S (x + y)

(+) :: Nat x -> Nat y -> Nat (x + y)
x + y = case x of
  Z_    -> y
  S_ x' -> S_ $ x' + y

makeTyFamFocus ''(+) "Add" "+:"

type family (x :: N) * (y :: N) :: N where
  Z   * y = Z
  S x * y = y + (x * y)

(*) :: Nat x -> Nat y -> Nat (x * y)
x * y = case x of
  Z_    -> Z_
  S_ x' -> y + (x' * y)

makeTyFamFocus ''(*) "Mul" "*:"

-- }}}

-- Addition {{{

addZ :: Nat x -> (x + Z) :~: x
addZ x = case x of
  Z_    -> begin (Z_ + Z_)
    =<| byDef   |>   Z_
  S_ x' -> begin (S_ (x' + Z_))
    =<| (sub _S |..  (x' + Z_)
    ..| addZ x' |>    x')
                |> S_ x'

addS :: Nat x -> Nat y -> S (x + y) :~: (x + S y)
addS x y = case x of
  Z_    -> begin   (S_ (Z_ + y))
    =<| (sub _S |..    (Z_ + y)
    ..| byDef   |>           y)
                |>        S_ y
    =<| byDef   |>  (Z_ + S_ y)
  S_ x' -> begin      (S_ (S_ x' + y))
    =<| (sub _S   |.. (S_  x' +    y)
    ..| byDef     |>   S_ (x' +    y)
    =<| addS x' y |>      (x' + S_ y))
                  |>   S_ (x' + S_ y)
    =<| byDef     |>  (S_  x' + S_ y)

addComm :: Nat x -> Nat y -> (x + y) :~: (y + x)
addComm x y = case x of
  Z_    -> begin  (Z_ + y)
    =<| byDef        |>   y
    =<| sym (addZ y) |>  (y + Z_)
  S_ x' -> begin         (S_ x' + y)
    =<| byDef        |>   S_ (x' + y)
    =<| (sub _S      |..     (x' + y)
    ..| addComm x' y |>      (y + x'))
                     |>   S_ (y + x')
    =<| addS y x'    |>   (y + S_ x')

addAssoc :: Nat x -> Nat y -> Nat z -> (x + (y + z)) :~: ((x + y) + z)
addAssoc x y z = case x of
  Z_    -> begin             (Z_ + y + z)
    =<| byDef            |>  (y  + z)
    =<| (sub (+: wit z)  |..  y
    ..| byDef            |>  (Z_ + y))
                         |> ((Z_ + y) + z)
  S_ x' -> begin             (S_   x' + (y  + z))
    =<| byDef            |>   S_  (x' + (y  + z))
    =<| (sub _S          |..      (x' + (y  + z))
    ..| addAssoc x' y z  |>      ((x' +  y) + z))
                         |>   S_ ((x' +  y) + z)
    =<| byDef            |>  (S_  (x' +  y) + z)
    =<| (sub (+: wit z)  |..  S_  (x' +  y)
    ..| byDef            |>  (S_   x' +  y))
                         |> ((S_   x' +  y) + z)

addSwap :: Nat x -> Nat y -> Nat z -> (x + (y + z)) :~: (z + (y + x))
addSwap x y z = case x of
  Z_    -> begin (y + z)
    =<| addComm y z |>      (z +  y)
    =<| (sub (wit z +:) |..       y
    ..| sym (addZ y)    |>       (y + Z_))
                        |>  (z + (y + Z_))
  S_ x' -> begin            (S_  x' + (y + z))
    =<| byDef           |>   S_ (x' + (y + z))
    =<| (sub _S         |..     (x' + (y + z))
    ..| addSwap x' y z  |>      (z  + (y + x')))
                        |>   S_ (z  + (y + x'))
    =<| addS z (y + x') |>    (z + S_ (y + x'))
    =<| (sub (wit z +:) |..        S_ (y + x')
    ..| addS y x'       |>          y + S_ x')
                        |>     z + (y + S_ x')

-- }}}

-- Multiplication {{{

mulZ :: Nat x -> (x * Z) :~: Z
mulZ x = case x of
  Z_    -> begin (Z_ * Z_)
    =<| byDef |>  Z_
  S_ x' -> begin   (S_ x' * Z_)
    =<| byDef   |> (Z_ + (x' * Z_))
    =<| byDef   |>       (x' * Z_)
    =<| mulZ x' |>  Z_

-- }}}

