{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Lens.Poly.TH where

import Control.Lens.Poly

import Language.Haskell.TH
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Syntax

import Control.Lens
import Language.Haskell.TH.Lens

import Control.Monad
import Data.List (stripPrefix, nub)

makePolyIso :: Q [Dec] -> Q [Dec]
makePolyIso = defSig >=> \(clNm,nExp,fNm,cs,cx,typ) -> do
  ts <- parseExpArgTypes nExp typ
  return
    [ InstanceD cx (appType clNm ts)
      [ FunD fNm cs
      ]
    ]

defSig :: Q [Dec] -> Q (Name,Int,Name,[Clause],Cxt,Type)
defSig = (=<<) $ \ds -> do
  case ds of
    -- [d]     -> (parsePolyDef d >>=)
    --   $  maybe noDef
    --   $ \(clNm,nExp,fNm,cs) ->
    --   return (clNm,nExp,fNm,cs,Nothing)
    [d1,d2] -> do
      md1 <- parsePolyDef  d1
      md2 <- parsePolyDef  d2
      mt1 <- parsePolyType d1
      mt2 <- parsePolyType d2
      (clNm,nExp,fNm,cs) <- maybe noDef return $ mplus md1 md2
      (cx,typ) <- maybe noSig return $ mplus mt1 mt2
      return (clNm,nExp,fNm,cs,cx,typ)
    _       -> fail "Unexpected Decs"
  where
  noDef = fail "Expected a Poly function definition"
  noSig = fail "Expected a Type Signature declaration"

parsePolyDef :: Dec -> Q (Maybe (Name,Int,Name,[Clause]))
parsePolyDef = \case
  ( preview _FunD ->
    Just ( ( stripSuffix "At" . view _Name 
      -> Just nm ) , cs) ) -> do
    (clNm,nExp,fNm) <- parseClassName nm
    return $ Just (clNm,nExp,fNm,cs)
  _ -> return Nothing

parsePolyType :: Dec -> Q (Maybe (Cxt,Type))
parsePolyType = \case
  ( preview _SigD -> Just (_,typ) )
    -> case typ of
      ForallT _ cx t -> return $ Just (cx,t)
      _              -> return $ Just ([],typ)
  _ -> return Nothing

parseExpArgTypes :: Int -> Type -> Q [Type]
parseExpArgTypes n typ = go typ
  where
  go = \case
    ConT _ | n == 0 -> return []
           | otherwise -> fail $ "Unsaturated type: " ++ pprint typ
    AppT t1 t2
      | n > 0 -> do
      ts <- go t1
      return $ ts ++ [t2]
      | otherwise -> fail $ "Oversaturated type: " ++ pprint typ
    t -> fail $ "Unexpected type: " ++ pprint t

conType :: Name -> [TyVarBndr] -> Con -> Type
conType tNm tVars cn = funType res ts & case tVars ++ vs of
  []  -> id
  vs' -> ForallT (nub vs') cx
  where
  res = resType tNm tVars
  (vs,cx,ts) = go cn
  go :: Con -> ([TyVarBndr],Cxt,[Type])
  go = \case
    NormalC _ (map snd -> ts) -> ([],[],ts)
    RecC _ (map (\(_,_,t) -> t) -> ts) -> ([],[],ts)
    InfixC (_,t1) _ (_,t2) -> ([],[],[t1,t2])
    ForallC vs cx cn' -> let (vs',cx',ts) = go cn'
      in (vs ++ vs',cx ++ cx',ts)

funType :: Type -> [Type] -> Type
funType = foldl (\t1 t2 -> ArrowT `AppT` t1 `AppT` t2)

appType :: Name -> [Type] -> Type
appType tNm = foldl AppT (ConT tNm)

resType :: Name -> [TyVarBndr] -> Type
resType tNm = appType tNm . map (VarT . view name)

-- PolyType {{{

data PolyType
  = PEquality
  | PIso
  | PLens
  | PPrism
  | PReview
  | PSetter
  | PGetter
  | PFold
  | PAction
  | PMonadicFold
  deriving (Eq,Ord,Show)

parseClassName :: String -> Q (Name,Int,Name)
parseClassName = \case
  "equality"    -> return (''PolyEquality    , 4 , 'equalityAt   )
  "iso"         -> return (''PolyIso         , 4 , 'isoAt        )
  "lens"        -> return (''PolyLens        , 4 , 'lensAt       )
  "prism"       -> return (''PolyPrism       , 4 , 'prismAt      )
  "review"      -> return (''PolyReview      , 4 , 'reviewAt     )
  "setter"      -> return (''PolySetter      , 4 , 'setterAt     )
  "getter"      -> return (''PolyGetter      , 2 , 'getterAt     )
  "fold"        -> return (''PolyFold        , 2 , 'foldAt       )
  "action"      -> return (''PolyAction      , 3 , 'actionAt     )
  "monadicFold" -> return (''PolyMonadicFold , 3 , 'monadicFoldAt)
  nm            -> fail $ "Unknown PolyType: " ++ nm

-- }}}

-- Util {{{

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix (reverse -> sfx) (reverse -> as) = 
    fmap reverse
  $ stripPrefix sfx as

_Name :: Lens' Name String
_Name = lens
  (\(Name (OccName s) _) -> s)
  (\(Name (OccName _) f) s ->
     Name (OccName s) f)

-- }}}

