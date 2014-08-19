{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Proxy.TH where

import Control.Applicative
import Data.Proxy
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as S
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Language.Haskell.Meta.Utils
import Language.Haskell.Meta.Syntax.Translate
import qualified Language.Haskell.Exts.Parser as Hs
import qualified Language.Haskell.Exts.Syntax as Hs
import qualified Language.Haskell.Exts.Extension as Hs

proxify :: QuasiQuoter
proxify = QuasiQuoter
  { quoteExp  = stub
  , quotePat  = stub
  , quoteType = stub
  , quoteDec  = declProxyFun
  }
  where
  stub = error "unimplemented"

declProxyFun :: String -> Q [Dec]
declProxyFun s = case parseHsDecls s of
  Left err -> fail err
  Right ds -> go ds
  where
  go []               = return []
  go (Hs.TypeSig _ nms t : ds) = do
    dss <- mapM (liftDecl t) nms
    rest <- go ds
    return $ concat $ rest : dss
  go (_ : ds)         = go ds

parseDeclsWith :: Hs.ParseMode -> String -> Either String [Hs.Decl]
parseDeclsWith m = fmap moduleDecls
  . parseResultToEither
  . Hs.parseModuleWithMode m

liftDecl :: Hs.Type -> Hs.Name -> Q [Dec]
liftDecl t (toName -> nm) = do
  t' <- quantify =<< proxifyType (toType t)
  cs <- mkDefClauses $ arityT t'
  return
    [ SigD nm t'
    , FunD nm cs
    ]

mkDefClauses :: Int -> Q [Clause]
mkDefClauses n = (:[]) <$> clause
  (replicate n wildP)
  (normalB (conE 'Proxy))
  []

proxifyType :: Type -> Q Type
proxifyType ty = case ty of
  ForallT vs c t -> ForallT vs c <$> proxifyType t
  SigT t k -> SigT <$> proxifyType t <*> pure k
  AppT (AppT ArrowT a) b ->
    AppT <$> (AppT ArrowT <$> proxifyType a) <*> proxifyType b
  _        -> appT [t|Proxy|] (return ty)

quantify :: Type -> Q Type
quantify ty = do
  vs <- freeVars ty
  case toList vs of
    []  -> return ty
    vs' -> return $ ForallT vs' [] ty

freeVars :: Type -> Q (Set TyVarBndr)
freeVars ty = case ty of
  ForallT vs _ t   -> S.difference (S.fromList vs) <$> freeVars t
  AppT f x         -> S.union <$> freeVars f <*> freeVars x
  SigT (VarT nm) k -> free $ KindedTV nm k
  SigT t _         -> freeVars t
  VarT nm          -> free $ PlainTV nm
  _                -> return S.empty
  where
  free :: TyVarBndr -> Q (Set TyVarBndr)
  free = return . S.singleton

parseMode :: Hs.ParseMode
parseMode = Hs.defaultParseMode
  { Hs.extensions =
    [ Hs.EnableExtension Hs.KindSignatures
    , Hs.EnableExtension Hs.TypeOperators
    ]
  }

