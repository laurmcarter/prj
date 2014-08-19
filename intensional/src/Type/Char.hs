{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Type.Char where

import Type.Context
import Type.Type
import Type.Value
import Type.Witness

import Type.List
import Type.Nat
import Type.Nat.TH

newtype Ch = C N
  deriving (Eq,Show)

data CharT (c :: Ch) where
  Char :: Nat n -> CharT (C n)

-- Witness {{{

type IsChar c = (WitBy c CharT, Reps c Char)

instance IsNat n => Witness (C n) where
  type Wit (C n) = CharT
  witness = Char witness

instance IsNat n => Value (C n) where
  type Val (C n) = Char
  toVal (Char n) = toEnum $ toVal n

-- }}}

type family ShowT t :: [Ch]

type instance ShowT Int       = [C_I,C_n,C_t]
type instance ShowT Bool      = [C_B,C_o,C_o,C_l]
type instance ShowT (Maybe t) = [C_M,C_a,C_y,C_b,C_e,C_space] :++ ShowT t

{-
showT :: (s ~ ShowT t, IsList s, AllWit CharT s, AllVal Char s) => Type t -> String
showT (t :: Type t) = go (witness :: List (ShowT t))
  where
  go :: (IsList cs, AllWit CharT cs, AllVal Char cs) => List cs -> String
  go ls = case ls of
    NilL      -> ""
    ConsL c l -> toVal c : go l
-}

-- C Types {{{

type C_space = C $(nat  32)

type C_0     = C $(nat  48)
type C_1     = C $(nat  49)
type C_2     = C $(nat  50)
type C_3     = C $(nat  51)
type C_4     = C $(nat  52)
type C_5     = C $(nat  53)
type C_6     = C $(nat  54)
type C_7     = C $(nat  55)
type C_8     = C $(nat  56)
type C_9     = C $(nat  57)

type C_A     = C $(nat  65)
type C_B     = C $(nat  66)
type C_C     = C $(nat  67)
type C_D     = C $(nat  68)
type C_E     = C $(nat  69)
type C_F     = C $(nat  70)
type C_G     = C $(nat  71)
type C_H     = C $(nat  72)
type C_I     = C $(nat  73)
type C_J     = C $(nat  74)
type C_K     = C $(nat  75)
type C_L     = C $(nat  76)
type C_M     = C $(nat  77)
type C_N     = C $(nat  78)
type C_O     = C $(nat  79)
type C_P     = C $(nat  80)
type C_Q     = C $(nat  81)
type C_R     = C $(nat  82)
type C_S     = C $(nat  83)
type C_T     = C $(nat  84)
type C_U     = C $(nat  85)
type C_V     = C $(nat  86)
type C_W     = C $(nat  87)
type C_X     = C $(nat  88)
type C_Y     = C $(nat  89)
type C_Z     = C $(nat  90)

type C_a     = C $(nat  97)
type C_b     = C $(nat  98)
type C_c     = C $(nat  99)
type C_d     = C $(nat 100)
type C_e     = C $(nat 101)
type C_f     = C $(nat 102)
type C_g     = C $(nat 103)
type C_h     = C $(nat 104)
type C_i     = C $(nat 105)
type C_j     = C $(nat 106)
type C_k     = C $(nat 107)
type C_l     = C $(nat 108)
type C_m     = C $(nat 109)
type C_n     = C $(nat 110)
type C_o     = C $(nat 111)
type C_p     = C $(nat 112)
type C_q     = C $(nat 113)
type C_r     = C $(nat 114)
type C_s     = C $(nat 115)
type C_t     = C $(nat 116)
type C_u     = C $(nat 117)
type C_v     = C $(nat 118)
type C_w     = C $(nat 119)
type C_x     = C $(nat 120)
type C_y     = C $(nat 121)
type C_z     = C $(nat 122)

-- }}}

-- C Type Proxies {{{

c_space = Type :: Type C_space

c_0     = Type :: Type C_0
c_1     = Type :: Type C_1
c_2     = Type :: Type C_2
c_3     = Type :: Type C_3
c_4     = Type :: Type C_4
c_5     = Type :: Type C_5
c_6     = Type :: Type C_6
c_7     = Type :: Type C_7
c_8     = Type :: Type C_8
c_9     = Type :: Type C_9

c_A     = Type :: Type C_A
c_B     = Type :: Type C_B
c_C     = Type :: Type C_C
c_D     = Type :: Type C_D
c_E     = Type :: Type C_E
c_F     = Type :: Type C_F
c_G     = Type :: Type C_G
c_H     = Type :: Type C_H
c_I     = Type :: Type C_I
c_J     = Type :: Type C_J
c_K     = Type :: Type C_K
c_L     = Type :: Type C_L
c_M     = Type :: Type C_M
c_N     = Type :: Type C_N
c_O     = Type :: Type C_O
c_P     = Type :: Type C_P
c_Q     = Type :: Type C_Q
c_R     = Type :: Type C_R
c_S     = Type :: Type C_S
c_T     = Type :: Type C_T
c_U     = Type :: Type C_U
c_V     = Type :: Type C_V
c_W     = Type :: Type C_W
c_X     = Type :: Type C_X
c_Y     = Type :: Type C_Y
c_Z     = Type :: Type C_Z

c_a     = Type :: Type C_a
c_b     = Type :: Type C_b
c_c     = Type :: Type C_c
c_d     = Type :: Type C_d
c_e     = Type :: Type C_e
c_f     = Type :: Type C_f
c_g     = Type :: Type C_g
c_h     = Type :: Type C_h
c_i     = Type :: Type C_i
c_j     = Type :: Type C_j
c_k     = Type :: Type C_k
c_l     = Type :: Type C_l
c_m     = Type :: Type C_m
c_n     = Type :: Type C_n
c_o     = Type :: Type C_o
c_p     = Type :: Type C_p
c_q     = Type :: Type C_q
c_r     = Type :: Type C_r
c_s     = Type :: Type C_s
c_t     = Type :: Type C_t
c_u     = Type :: Type C_u
c_v     = Type :: Type C_v
c_w     = Type :: Type C_w
c_x     = Type :: Type C_x
c_y     = Type :: Type C_y
c_z     = Type :: Type C_z

-- }}}

