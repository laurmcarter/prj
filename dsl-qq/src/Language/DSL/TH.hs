{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.DSL.TH where

import Language.DSL
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Word

-- D {{{

instance D Dec where
  type TypeD Dec = Type
  type PatD  Dec = Pat
  type ExpD  Dec = Exp
  fun nm ps e = FunD nm [Clause ps (NormalB e) []]
  val p e     = ValD p (NormalB e) []
  sigD        = SigD

-- }}}

-- T K C {{{

instance T Type where
  type KindT Type = Kind
  type CxtT  Type = Pred
  type LitT  Type = TyLit
  forallT = ForallT . map mkTyVar
    where
    mkTyVar (nm,mk) = maybe (PlainTV nm) (KindedTV nm) mk
  appT   = AppT
  sigT   = SigT
  varT   = VarT
  conT   = ConT
  arrowT = ArrowT
  litT   = LitT

instance TLCommon TyLit where
  stringTL = StrTyLit
  intTL    = NumTyLit

instance K Kind where
  forallK ns = ForallT (map PlainTV ns) []
  varK       = VarT
  conK       = ConT
  arrowK a b = ArrowT `AppT` a `AppT` b
  starK      = StarT

instance C Pred where
  type TypeC Pred = Type
  equalP = EqualP
  classP = ClassP

-- }}}

-- P E {{{

instance P Pat where
  type LitP  Pat = Lit
  type LitsP Pat = '[LString,LNum,LPrim]
  litP  = LitP
  varP  = VarP
  conP  = ConP
  wildP = WildP

instance E Exp where
  type TypeE Exp = Type
  type PatE  Exp = Pat
  type LitE  Exp = Lit
  type LitsE Exp = '[LString,LNum,LPrim]
  varE    = VarE
  conE    = ConE
  litE    = LitE
  appE    = AppE
  lam     = LamE
  case_ e = CaseE e . map mkMatch
    where
    mkMatch (p,e) = Match p (NormalB e) []
  sigE    = SigE

-- }}}

-- L {{{

instance LString Lit where
  charL     = CharL
  stringL   = StringL

instance LNum Lit where
  integerL  = IntegerL
  rationalL = RationalL

instance LPrim Lit where
  intPrimL    = IntPrimL
  wordPrimL   = WordPrimL
  floatPrimL  = FloatPrimL
  doublePrimL = DoublePrimL
  stringPrimL = StringPrimL

-- }}}

