{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}

module Data.Constraint.Instance where

import Data.Constraint
import Data.Constraint.Unsafe

import Language.Haskell.TH
import Language.Haskell.TH.Lens
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse (parseType)

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Unify
import Control.Lens
import Data.Maybe
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

mentions :: Type -> Type -> Bool
mentions t typ = elem t
  $ universe typ

{-
deriveRecovery :: String -> Q [Dec]
deriveRecovery s = do
  typ <- either fail return $ parseType s
  let (vs,cl,ts) = unappT typ
  case cl ^? _ConT of
    Just clNm -> do
      inf <- reify clNm
      ds <- case inf ^? _ClassI._2 of
        Just ds -> return ds
        _ -> fail $ "Not a class: " ++ show clNm
      let relevant = flip filter ds
            $ \case
              (preview (_InstanceD._2) -> Just t)
      -- is <- reifyInstances clNm ts
      -- runIO $ print inf >> print ts
      return []
    _         -> fail "Not a class signature"
-}

unappT :: Type -> ([TyVarBndr],Type,[Type])
unappT = fix $ \go -> \case
  (preview _AppT
    -> Just (f,x))      -> go f & _3 %~ (x:)
  (preview _ForallT
    -> Just (vs,_cx,t)) -> go t & _1 %~ (vs++)
  t -> ([],t,[])

-- Generic transformation {{{

data TypeF a
  = ForallT_ [TyVarBndrF a] (CxtF a) (TypeF a)
  | AppT_ (TypeF a) (TypeF a)
  | SigT_ (TypeF a) (TypeF a)
  | VarT_ a
  | ConT_ a
  | PromotedT_ a
  | TupleT_ Int
  | UnboxedTupleT_ Int
  | ArrowT_
  | ListT_
  | PromotedTupleT_ Int
  | PromotedNilT_
  | PromotedConsT_
  | StarT_
  | ConstraintT_
  | LitT_ TyLit
  deriving (Functor,Foldable,Traversable)

data TyVarBndrF a
  = PlainTV_ a
  | KindedTV_ a (TypeF a)
  deriving (Functor,Foldable,Traversable)

type CxtF a = [PredF a]

data PredF a
  = ClassP_ a [TypeF a]
  | EqualP_ (TypeF a) (TypeF a)
  deriving (Functor,Foldable,Traversable)

makePrisms ''TypeF
makePrisms ''TyVarBndrF
makePrisms ''PredF

newtype UName = UName
  { uName :: Either Int Name
  }

makeLensesWith
  ( isoRules
  & lensIso .~ (Just . ('_' :))
  ) ''UName

newtype UType = UType
  { uType :: TypeF UName
  }

makeLensesWith
  ( isoRules
  & lensIso .~ (Just . ('_' :))
  ) ''UType

instance Partial UName where
  unknown   = UName . Left
  isUnknown = \case
    UName (Left i) -> Just i
    _              -> Nothing
  unknowns (UName e) = case e of
    Left i  -> [i]
    Right t -> []
  s $? n@(UName e) = case e of
    Left  i -> case H.lookup i $ runSubstitution s of
      Just t -> n
      _      -> UName e
    _ -> n

_UTypeUnkn :: Traversal' UType Unknown
_UTypeUnkn = from _UType
  . (_VarT_ `failing` _ConT_ `failing` _PromotedT_)
  . from _UName
  . _Left

{-
instance Partial UType where
  unknown = UType . VarT_ . unknown
  isUnknown = preview _UTypeUnkn
  unknowns = toListOf _UTypeUnkn
  s $? t = t
    & _UTypeUnkn
  
-}

{-
gen :: Type -> UType
gen = fromConc . fromType

inst :: UType -> Maybe Type
inst = toConc >=> toType

fromConc :: TypeF UType -> UType
fromConc = UType . Right

toConc :: UType -> Maybe (TypeF UType)
toConc (UType e) = case e of
  Right t -> Just t
  _       -> Nothing

fromType :: Type -> TypeF UType
fromType = \case
  ForallT vs cx t  -> ForallT_ vs (map fromPred cx) $ gen t
  AppT f x         -> AppT_ (gen f) (gen x)
  SigT t k         -> SigT_ (gen t) (gen k)
  VarT nm          -> VarT_ nm
  ConT nm          -> ConT_ nm
  PromotedT nm     -> PromotedT_ nm
  TupleT n         -> TupleT_ n
  UnboxedTupleT n  -> UnboxedTupleT_ n
  ArrowT           -> ArrowT_
  ListT            -> ListT_
  PromotedTupleT n -> PromotedTupleT_ n
  PromotedNilT     -> PromotedNilT_
  PromotedConsT    -> PromotedConsT_
  StarT            -> StarT_
  ConstraintT      -> ConstraintT_
  LitT l           -> LitT_ l

toType :: TypeF UType -> Maybe Type
toType = \case
  ForallT_ vs cx t  -> ForallT vs <$> (mapM toPred cx) <*> inst t 
  AppT_ f x         -> AppT <$> inst f <*> inst x
  SigT_ t k         -> SigT <$> inst t <*> inst k
  VarT_ nm          -> Just $ VarT nm
  ConT_ nm          -> Just $ ConT nm
  PromotedT_ nm     -> Just $ PromotedT nm
  TupleT_ n         -> Just $ TupleT n
  UnboxedTupleT_ n  -> Just $ UnboxedTupleT n
  ArrowT_           -> Just $ ArrowT
  ListT_            -> Just $ ListT
  PromotedTupleT_ n -> Just $ PromotedTupleT n
  PromotedNilT_     -> Just $ PromotedNilT
  PromotedConsT_    -> Just $ PromotedConsT
  StarT_            -> Just $ StarT
  ConstraintT_      -> Just $ ConstraintT
  LitT_ l           -> Just $ LitT l

fromPred :: Pred -> PredF UType
fromPred = \case
  ClassP n ts  -> ClassP_ n $ map gen ts
  EqualP t1 t2 -> EqualP_ (gen t1) (gen t2)

toPred :: PredF UType -> Maybe Pred
toPred = \case
  ClassP_ n ts  -> ClassP n <$> mapM inst ts
  EqualP_ t1 t2 -> EqualP <$> inst t1 <*> inst t2
-}

-- }}}

