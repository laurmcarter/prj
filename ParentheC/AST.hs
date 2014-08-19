
module ParentheC.AST where

import Data.ByteString.Lazy
import Data.List.NonEmpty

data Prog = Prog (NonEmpty Expr)

data Expr
  = DefineRegisters [Var]
  | DefineProgramCounter Var
  | DefineLabel LName Complex
  | DefineUnion UName [UPat]
  | DefineMain Complex
  deriving (Eq,Show)

type LName = ByteString
type UName = ByteString
type Var = ByteString

data Complex
  = Simple Simple
  | MountTrampoline Simple Simple Simple
  | DismountTrampoline Simple
  | IfC Simple Complex Complex
  | Cond [(Simple,Complex)] Complex
  | Begin [Complex]
  | Set Var Simple
  | UnionCase Var UName [(UPat,Complex)]
  | Let [(Var,Simple)] Complex
  deriving (Eq,Show)

data UPat = UPat UCon [Var]
  deriving (Eq,Show)

type UCon = ByteString

data Simple
  = T
  | F
  | Var Var
  | I Int
  | Op1 Op1 Simple
  | Op2 Op2 Simple Simple
  | IfS Simple Simple Simple
  | UCon UName UCon [Simple]
  deriving (Eq,Show)

data Op1
  = IsZero
  | Not
  | Sub1
  | Add1
  | Error
  | Random
  deriving (Eq,Show)

data Op2
  = Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | LT
  | GT
  | LET
  | GET
  deriving (Eq,Show)

