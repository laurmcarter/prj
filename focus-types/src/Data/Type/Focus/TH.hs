{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Type.Focus.TH
  ( makeTyFamFocusWith
  , declareFocusWith
  , makePromotedEqualityWith
  , makeTyFamFoci
  , makeTyFamFocus
  , makeTyFamFocusOnly
  , declareFoci
  , declareFocus
  , declareFocusOnly
  , makePromotedEquality
  , makePromotedEqualities
  ) where

import Data.Type.Focus

import Control.Applicative
import Control.Lens hiding ((|>))
import Data.Type.Equality
import Language.Haskell.TH.Lens
import Language.Haskell.TH.Syntax

import Control.Monad ( replicateM , forM )
import Data.Char hiding (chr)
import Data.List     ( nub )
import Data.Map      ( fromList )
import Data.Monoid   ( First )

-- Exported

-- Convenience {{{

makeTyFamFoci :: [(Name,String,String)] -> Q [Dec]
makeTyFamFoci = fmap concat . mapM (\(x,y,z) -> makeTyFamFocus x y z)

makeTyFamFocus :: Name -> String -> String -> Q [Dec]
makeTyFamFocus fNm tNm cNm = makeTyFamFocusWith (Just cNm) fNm tNm

makeTyFamFocusOnly :: Name -> String -> Q [Dec]
makeTyFamFocusOnly = makeTyFamFocusWith Nothing

declareFoci :: [Q [Dec]] -> Q [Dec]
declareFoci = fmap concat . mapM declareFocus

declareFocus :: Q [Dec] -> Q [Dec]
declareFocus = declareFocusWith Nothing

declareFocusOnly :: Q [Dec] -> Q [Dec]
declareFocusOnly = declareFocusWith Nothing

makePromotedEqualities :: [Name] -> Q [Dec]
makePromotedEqualities = fmap concat . mapM makePromotedEquality

makePromotedEquality :: Name -> Q [Dec]
makePromotedEquality = makePromotedEqualityWith

-- }}}

-- Base Definitions {{{

makeTyFamFocusWith :: Maybe String
  -> Name -> String -> Q [Dec]
makeTyFamFocusWith mc fNm' tNm' = do
  inf <- reify fNm'
  (fNm,fVars) <- case inf ^? _FamilyI._1 of
    Just fam -> case fam of
      (preview _FamilyD -> Just (flv,fNm,fVars,_)) -> do
        mPreview "Not a type family Dec" flv _TypeFam
        return (fNm,fVars)
      (preview _ClosedTypeFamilyD -> Just (fNm,fVars,_,_)) -> return (fNm,fVars)
      _ -> fail "Not a Type Family"
    _ -> fail "Not a Family Dec"
  ----------------------------------------
  let tNm = mkName tNm'
  wrapNm <- newName "p"
  let fTyps     = map (VarT . view name) fVars
  let fWrapVars = (_PlainTV # wrapNm) : fVars
  let typ       = return $ VarT wrapNm `AppT` appsT (ConT fNm) fTyps
  ----------------------------------------
  let mf = fmap (\cNm -> familyCong (mkName cNm) fNm fVars) mc
  mkFocusWith mf tNm fWrapVars typ

declareFocusWith :: Maybe String -> Q [Dec] -> Q [Dec]
declareFocusWith mn = (=<<) $ \ds -> do
  (tNm,tVars,fldTyp) <- mPreview "Not a TySyn Dec" ds $ _head._TySynD
  let mf = fmap (\cNm -> const $ dataCong (mkName cNm) tNm tVars) mn
  mkFocusWith mf tNm tVars $ return fldTyp

makePromotedEqualityWith :: Name -> Q [Dec]
makePromotedEqualityWith tNm' = do
  inf <- reify tNm'
  case inf ^? _TyConI of
    Just d -> case d of
      (preview _NewtypeD -> Just (_cx,_tNm,_tVars,tCon,_tDrv))
        -> promotedCong tCon
      (preview _DataD    -> Just (_cx,_tNm,_tVars,tCons,_tDrv))
        -> concat <$> mapM promotedCong tCons
      _ -> fail "Not a Data or Newtype Dec"
    _ -> fail "Not a Type Constructor"

-- }}}

-- Internal

-- Congruences / Equalities {{{

familyCong :: Name -> Name -> [TyVarBndr] -> Name -> Q [Dec]
familyCong cNm fNm tVars isoNm = do
  let n = length tVars
  -- SigD --------------------------------
  congTyp <- mkCongTyp n $ ConT fNm
  -- FunD --------------------------------
  congDef <- mkCongDef n $ opE (VarE '(>-)) (VarE isoNm)
  ----------------------------------------
  return
    [ SigD cNm congTyp
    , FunD cNm congDef
    ]

dataCong :: Name -> Name -> [TyVarBndr] -> Q [Dec]
dataCong cNm tNm tVars = do
  let n = length tVars
  -- SigD --------------------------------
  congTyp <- mkCongTyp n $ ConT tNm
  -- FunD --------------------------------
  congDef <- mkCongDef n id
  ----------------------------------------
  return
    [ SigD cNm congTyp
    , FunD cNm congDef
    ]

promotedCong :: Con -> Q [Dec]
promotedCong tCon = do
  cNm <- newName $ '_' : nm^._Name
  let n = go tCon
  -- SigD --------------------------------
  congTyp <- mkCongTyp n $ PromotedT nm
  -- FunD --------------------------------
  congDef <- mkCongDef n id
  ----------------------------------------
  return
    [ SigD cNm congTyp
    , FunD cNm congDef
    ]
  where
  nm :: Name
  nm = tCon ^. name
  go :: Con -> Int
  go = \case
    NormalC _ ts       -> length ts
    RecC    _ ts       -> length ts
    InfixC  _ _  _     -> 2
    ForallC _ _  tCon' -> go tCon'

{-
mkCongName :: (String -> String) -> (String -> String) -> Name -> Name
mkCongName symf conf tNm = mkName
  $ (if isSymOcc tNm then symf else conf)
  $ tNm^.name._Name
-}

mkCongTyp :: Int -> Type -> Q Type
mkCongTyp n cTyp = do
  [(ts1,t1),(ts2,t2)] <- replicateM 2 $ do
    ts <- replicateM n $ fmap VarT $ newName "x"
    return ( ts , appsT cTyp ts )
  let typ = resT ( zipWith (opT $ ConT ''(:~:)) ts1 ts2)
                 $ opT (ConT ''(:~:)) t1 t2
  let varBndrs = map (review _PlainTV) $ nub $ typ ^.. typeVars
  return $ if null varBndrs
    then typ
    else ForallT varBndrs [] typ

mkCongDef :: Int -> (Exp -> Exp) -> Q [Clause]
mkCongDef n f = do
  xs <- replicateM n $ newName "x"
  let vs = map VarE xs
  let ps = map VarP xs
  let congBody = f $ foldl (opE $ VarE '(@:)) (ConE 'Refl) vs
  return [ Clause ps (NormalB congBody) [] ]

-- }}}

-- Focus {{{

mkFocusWith :: Maybe (Name -> Q [Dec])
  -> Name -> [TyVarBndr]
  -> Q Type -> Q [Dec]
mkFocusWith mf tNm tVars typ = do
  let (conNm,decNm,isoNm) = mkFocusNames tNm
  ----------------------------------------
  (++) <$> mkFocusNewtype   tNm conNm decNm tVars typ
       <*> ((++)
       <$> mkFocusIso isoNm tNm conNm decNm tVars typ
       <*> maybe ( pure [] ) ( $ isoNm ) mf )

mkFocusNewtype :: Name -> Name -> Name -> [TyVarBndr] -> Q Type -> Q [Dec]
mkFocusNewtype tNm conNm decNm tVars mtyp = do
  typ <- mtyp
  return
    [ NewtypeD [] tNm tVars
      ( RecC conNm [ (decNm,NotStrict,typ) ] )
      []
    ]

mkFocusNames :: Name -> (Name,Name,Name)
mkFocusNames tNm =
  ( mkName nm
  , mkName $ "un" ++ nm
  , mkName $ '_'   : nm
  )
  where
  nm = tNm ^. _Name & _head %~ toUpper

mkFocusIso :: Name -> Name -> Name -> Name -> [TyVarBndr] -> Q Type -> Q [Dec]
mkFocusIso isoNm tNm conNm decNm tVars mtyp = do
  typ <- mtyp
  let isoBody = opE (VarE 'iso) (ConE conNm) (VarE decNm)
  let tvarNms  = map (view name) tVars
  [(t1,t3),(t2,t4)] <- replicateM 2 $ do
    tTyps <- forM tvarNms $ fmap VarT . newName . view _Name
    let varMap     = fromList $ zip tvarNms tTyps
    let wrappedTyp = appsT (ConT tNm) tTyps
    return
      ( substType varMap typ
      , substType varMap wrappedTyp
      )
  let freeVars = nub $ ( t1 ^.. typeVars ) ++ ( t2 ^.. typeVars )
  let varBndrs = map (review _PlainTV) freeVars
  return
    [ SigD isoNm $ (if null varBndrs then id else ForallT varBndrs []) $ appsT (ConT ''Iso) [t1,t2,t3,t4]
    , FunD isoNm [ Clause [] (NormalB isoBody) [] ]
    ]

-- }}}

-- Util {{{

_Name :: Lens' Name String
_Name = lens
  (\(Name (OccName s) _) -> s)
  (\(Name (OccName _) f) s -> Name (OccName s) f)

mPreview :: Monad m => String -> s -> Getting (First a) s a -> m a
mPreview msg s p = maybe (fail msg) return $ preview p s

appsT :: Type -> [Type] -> Type
appsT = foldl AppT

opT :: Type -> Type -> Type -> Type
opT o x y = o `AppT` x `AppT` y

opE :: Exp -> Exp -> Exp -> Exp
opE o x y = o `AppE` x `AppE` y

resT :: [Type] -> Type -> Type
resT ts t = foldr (opT $ ConT ''(->)) t ts

-- }}}

