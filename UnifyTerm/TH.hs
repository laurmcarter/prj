{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module UnifyTerm.TH where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Text.Show.Pretty (ppShow)
import Control.Lens.TH
import Control.Applicative
import Data.List

import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations

makeLensesForDatFam :: Name -> Q [Dec]
makeLensesForDatFam famName = do
  famInfo <- reify famName
  case famInfo of
    FamilyI (FamilyD DataFam datFamName args _) insts ->
      case filter hasSingleCons insts of
        [DataInstD ctx tyConName args' cons _] -> case cons of
          [NormalC dataConName [(    _,ty)]]
            -> makeIsoLenses lensRules ctx tyConName args dataConName Nothing ty
          [RecC    dataConName [(fld,_,ty)]]
            -> makeIsoLenses lensRules ctx tyConName args dataConName (Just fld) ty
          [RecC    dataConName _ ] -> do
            newTyName <- qNewName "Pkg"
            let newNames = map getNameT args'
                oldNames = map getNameTV args
                reboundArgs = reBind oldNames newNames args
            ([ TySynD newTyName [] (AppT (ConT tyConName) (head args')) ] ++) <$>
              makeFieldLenses lensRules ctx newTyName (tail reboundArgs) cons
          _ -> fail "makeLensesForDatFam: Unsupported data instance"
        _  -> fail "makeLensesForDatFam: Unsupported data instance"
    _ -> fail "makeLensesForDatFam: Expected data family"

reBind :: Biplate from Name => [Name] -> [Name] -> from -> from
reBind olds news from = foldr (\(old,new)-> transformBi (\n -> if n == old then new else n)) from $ zip olds news

findClassVars :: Info -> Q [Name]
findClassVars info = case info of
  ClassI (ClassD _ _ vs _ _) _ -> return $ map getNameTV vs
  _ -> fail "findClassVars: Expected single class description"

getNameT v = case v of
  VarT n -> n
  ConT n -> n

getNameTV v = case v of
  PlainTV n    -> n
  KindedTV n _ -> n

hasSingleCons :: InstanceDec -> Bool
hasSingleCons ins = case ins of
  DataInstD ctx name typs [rec] nms -> True
  _ -> False

successFun :: String -> Q [Dec]
successFun info = [d| testResult = info |]

