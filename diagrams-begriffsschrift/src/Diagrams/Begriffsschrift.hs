{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams.Begriffsschrift where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Arrow

type Var = String

data Prop a
  = Atom  a
  | Var   Var
  | Impl      (Prop a) (Prop a)
  | Neg       (Prop a)
  | Quant Var (Prop a)
  | Eql       (Prop a) (Prop a)
  deriving (Eq,Show)

type Dgm = Diagram B R2

