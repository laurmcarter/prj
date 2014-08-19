{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Transform.Recursive.TH where

import Data.Transform.Recursive

import Control.Applicative
import Control.Lens
import Control.Monad ((>=>),(<=<))
import Language.Haskell.TH
import Language.Haskell.TH.Lens
import Language.Haskell.TH.Syntax

deriveRecursive :: Name -> Q [Dec]
deriveRecursive n = do
  i <- reify n
  case i of
    (nty -> TC nm xs c) -> do
      cxt <- checkInst nm fieldType
      ds <- [d| instance Recursive $instType where
                  _Recur f = out_ (_Recur (in_ f))
                    where
                    decon  = $getDecon
                    con    = $getCon
                    in_  g = fmap decon . g . con
                    out_ g = fmap con   . g . decon
              |]
      traverseOf (each . _InstanceD . _1) (const $ return cxt) ds
      where
      getCon, getDecon :: ExpQ
      getCon   = conE $ c ^. name
      getDecon = do
        x <- newName "x"
        [| \ $(conP (c ^. name) [varP x]) -> $(varE x) |]
      ----
      fieldType :: Type
      fieldType = view (singular conFields . _2) c
      ----
      instType :: TypeQ
      instType = appsT (conT nm) $ map bndrVar xs

    (dat -> TC nm xs cs) -> do
      (f,e,body) <- recDec
      [d| instance Recursive $instType where
            _Recur $f $e = $body
        |]
{-
      ds <- [d| instance Recursive $instType where
                  _Recur f = $(return undefined)
              |]
      traverseOf (each . _InstanceD . _1) (const $ return cxt) ds
-}

      where
      instType :: TypeQ
      instType = appsT (conT nm) $ map bndrVar xs
      ----
      recDec :: Q (PatQ,PatQ,ExpQ)
      recDec = do
        [f,e] <- mapM newName ["f","e"]
        t     <- instType
        let body = caseE (varE e) $ map (mkCase f t) cs
        return (varP f,varP e,body)

    _ -> fail $ "Unsupported: " ++ show n
    where
    nty = tc _NewtypeD
    dat = tc _DataD
    tc = preview . (_TyConI .)

pattern TC n xs c <- Just (_,n,xs,c,_)

checkInst :: Name -> Type -> CxtQ
checkInst nm t = case t of
  (toListOf typeVars -> (:) {}) -> cxt $ single $ classP ''Recursive [return t]
  _ -> do
    is <- reifyInstances ''Recursive [t]
    if null is
      then fail noInst
      else return []
  where
  noInst = unwords
    [ "No instance for (Recursive"
    , pprint t ++ ")"
    , "when constructing instance (Recursive"
    , pprint nm ++ ")"
    ]

mkCase :: Name -> Type -> Con -> MatchQ
mkCase f ty c = do
  xs <- mapM (newName . single) $ take (length ts) ['a'..]
  match
    (conP n  $ map varP xs)
    (normalB $ body $ zip xs ts)
    []
  where
  n    = c ^.  name
  ts   = c ^.. conFields._2
  body = foldl app_ [| pure $(conE n) |]
  app_ e ((varE -> a),t)
    | t == ty = [| $e <*> $(varE f) $a |]
    | True    = [| $e <*> pure      $a |]

singleM :: Monad m => m a -> m [a]
singleM = (return . single =<<)

single :: a -> [a]
single = (:[])

appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT c = foldl appT c

bndrVar :: TyVarBndr -> TypeQ
bndrVar = varT . view name

makeIso' :: Name -> DecsQ
makeIso' = makeLensesWith
  $ isoRules
  & lensIso .~ (\s -> Just $ '_' : s)


