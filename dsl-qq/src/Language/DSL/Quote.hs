{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.DSL.Quote where

import Language.DSL
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Proxy
import GHC.Exts (Constraint)

-- TH data

-- Dec {{{

-- FunD Name [Clause]
-- ValD Pat Body [Dec]
-- DataD Cxt Name [TyVarBndr] [Con] [Name]
-- NewtypeD Cxt Name [TyVarBndr] Con [Name]
-- TySynD Name [TyVarBndr] Type
-- ClassD Cxt Name [TyVarBndr] [FunDep] [Dec]
-- InstanceD Cxt Type [Dec]
-- SigD Name Type
-- ForeignD Foreign
-- InfixD Fixity Name
-- PragmaD Pragma
-- FamilyD FamFlavour Name [TyVarBndr] (Maybe Kind)
-- DataInstD Cxt Name [Type] [Con] [Name]
-- NewtypeInstD Cxt Name [Type] Con [Name]
-- TySynInstD Name TySynEqn
-- ClosedTypeFamilyD Name [TyVarBndr] (Maybe Kind) [TySynEqn]
-- RoleAnnotD Name [Role]

-- }}}

-- Type ( / Kind ) {{{

-- ForallT [TyVarBndr] Cxt Type
-- AppT Type Type
-- SigT Type Kind
-- VarT Name
-- ConT Name
-- PromotedT Name
-- TupleT Int
-- UnboxedTupleT Int
-- ArrowT
-- ListT
-- PromotedTupleT Int
-- PromotedNilT
-- PromotedConsT
-- StarT
-- ConstraintT
-- LitT TyLit

-- }}}

-- Pred {{{

-- ClassP Name [Type]
-- EqualP Type Type

-- }}}

-- Pat {{{

-- LitP Lit
-- VarP Name
-- TupP [Pat]
-- UnboxedTupP [Pat]
-- ConP Name [Pat]
-- InfixP Pat Name Pat
-- UInfixP Pat Name Pat
-- ParensP Pat
-- TildeP Pat
-- BangP Pat
-- AsP Name Pat
-- WildP
-- RecP Name [FieldPat]
-- ListP [Pat]
-- SigP Pat Type
-- ViewP Exp Pat

-- }}}

-- Exp {{{

-- VarE Name
-- ConE Name
-- LitE Lit
-- AppE Exp Exp
-- InfixE (Maybe Exp) Exp (Maybe Exp)
-- UInfixE Exp Exp Exp
-- ParensE Exp
-- LamE [Pat] Exp
-- LamCaseE [Match]
-- TupE [Exp]
-- UnboxedTupE [Exp]
-- CondE Exp Exp Exp
-- MultiIfE [(Guard, Exp)]
-- LetE [Dec] Exp
-- CaseE Exp [Match]
-- DoE [Stmt]
-- CompE [Stmt]
-- ArithSeqE Range
-- ListE [Exp]
-- SigE Exp Type
-- RecConE Name [FieldExp]
-- RecUpdE Exp [FieldExp]

-- }}}

-- Misc {{{

-- type Kind = Type
-- type Cxt  = [Pred]

-- * TyVarBndr *
-- PlainTV Name
-- KindedTV Name Kind

-- * Body *
-- GuardedB [(Guard,Exp)]
-- NormalB Exp

-- * Clause *
-- Clause [Pat] Body [Dec]

-- * Match *
-- Match Pat Body [Dec]

-- * Guard *
-- NormalG Exp
-- PatG [Stmt]

-- * Stmt *
-- BindS Pat Exp
-- LetS [Dec]
-- NoBindS Exp
-- ParS [[Stmt]]

-- * Range *
-- FromR Exp
-- FromThenR Exp Exp
-- FromToR Exp Exp
-- FromThenToR Exp Exp Exp

-- * Role *
-- NominalR
-- RepresentationalR
-- PhantomR
-- InferR

-- * TySynEqn *
-- TySynEqn [Type] Type

-- * Con *
-- NormalC Name [StrictType]
-- RecC Name [VarStrictType]
-- InfixC StrictType Name StrictType
-- ForallC [TyVarBndr] Cxt Con

-- * Strict *
-- IsStrict
-- NotStrict
-- Unpacked

-- type StrictType = (Strict,Type)
-- type VarStrictType = (Name,Strict,Type)

-- }}}

{-
thExp :: forall t. TH E t => Exp -> t
thExp = \case
  VarE nm         -> varE nm
  ConE nm         -> conE nm
  LitE l          -> litE $ thLitE (Proxy :: Proxy t) l
  AppE f x        -> appE (thExp f) (thExp x)
  ParensE e       -> thExp e
  LamE ps e       -> lam (map thPat ps) $ thExp e
  LamCaseE ms     -> undefined
  TupE es         -> undefined
  UnboxedTupE es  -> undefined
  CondE t c a     -> undefined
  MultiIfE bs     -> undefined
  LetE ds e       -> undefined
  CaseE e ms      -> undefined
  DoE ss          -> undefined
  CompE ss        -> undefined
  ArithSeqE r     -> undefined
  ListE es        -> undefined
  SigE e t        -> undefined
  InfixE ma op mb -> undefined
  UInfixE a op b  -> undefined
  -- deferred
  RecConE nm fs   -> undefined
  RecUpdE e fs    -> undefined
-}

{-
thLitE :: TH E t => proxy t -> Lit -> LitE t
thLitE = thLit (Proxy :: Proxy E)

thLitP :: TH P t => proxy t -> Lit -> LitP t
thLitP = thLit (Proxy :: Proxy P)

thLit :: (L (LitType c t), LPrim (LitType c t))
  => p0 c -> p1 t -> Lit -> LitType c t
thLit _ _ = \case
  CharL       c  -> charL c
  StringL     s  -> stringL s
  IntegerL    n  -> integerL n
  RationalL   n  -> rationalL n
  IntPrimL    n  -> intPrimL n
  WordPrimL   n  -> wordPrimL n
  FloatPrimL  n  -> floatPrimL n
  DoublePrimL n  -> doublePrimL n
  StringPrimL ws -> stringPrimL ws
-}

{-
thPat :: forall t. TH P t => Pat -> t
thPat = \case
  LitP l         -> litP $ thLitP (Proxy :: Proxy t) l
  VarP nm        -> varP nm
  TupP ps        -> undefined
  UnboxedTupP ps -> undefined
  ConP nm ps     -> undefined
  InfixP a op b  -> undefined
  UInfixP a op b -> undefined
  ParensP p      -> undefined
  TildeP p       -> undefined
  BangP p        -> undefined
  AsP nm p       -> undefined
  WildP          -> undefined
  RecP n fs      -> undefined
  ListP ps       -> undefined
  SigP p t       -> undefined
  ViewP e p      -> undefined
-}

