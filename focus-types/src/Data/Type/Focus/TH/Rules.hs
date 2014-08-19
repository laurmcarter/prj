{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Type.Focus.TH.Rules where

import Prelude hiding (elem)
-- import Control.Lens
-- import Data.Char.Name
import Control.Applicative
import Data.Type.Bool
import Data.Type.Equality
import Data.Set (Set)
import qualified Data.Set as S
import Data.Proxy
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data FocusSource (sup :: [Feature]) (src :: Source) where
  TypeFamily :: FocusSource '[MkNt,MkIso,MkCong] FromFam  -- type family (x :: N) + (y :: N) :: N
  TypeSig    :: FocusSource '[MkNt,MkIso,MkCong] FromSig  -- type AddZ x = (x + Z) :~: x
                                                            -- type L p a f = p (f a)
  Promoted   :: FocusSource '[     MkIso,MkCong] FromProm -- data N = Z | S N

data Source
  = FromFam
  | FromSig
  | FromProm
  deriving (Eq,Show)

type instance x == y = SourceEq x y
type family SourceEq (x :: Source) (y :: Source) :: Bool where
  SourceEq FromFam  FromFam  = True
  SourceEq FromSig  FromSig  = True
  SourceEq FromProm FromProm = True
  SourceEq x        y        = False

data Feature
  = MkNt   -- Add p x y = Add { unAdd :: p (x + y) }
  | MkIso  -- add  :: Iso (p (w + x)) (q (y + z)) (Add p w x) (Add q y z)
  | MkCong -- (+:) :: (w :~: y) -> (x :~: z) -> (w + x) :~: (y + z)
  deriving (Eq,Ord,Show)

data FocusFeature (fts :: [Feature]) (src :: Source) (req :: [Feature]) (a :: *) (b :: *) where
  MakeNewtype    :: FocusFeature '[MkNt]
    src
    '[]
    a
    (Either String (String -> Maybe String) -> a)
  ----
  MakeIso        :: FocusFeature '[MkIso]
    src
    '[]
    a
    (Either String (String -> Maybe String) -> a)
  ----
  MakeCongruence :: FocusFeature '[MkCong]
    src
    (If (src == FromFam) '[MkIso] '[])
    a
    (When (src == FromFam) String (String -> Maybe String) -> a)
  (:&) :: Subset req f1
       => FocusFeature f1         src '[] b c
       -> FocusFeature f2         src req a b
       -> FocusFeature (f1 ++ f2) src '[] a c

mkFocus :: forall fs sup src b. (Subset fs sup) => FocusSource sup src -> FocusFeature fs src '[] (Q [Dec]) b
  -> Name -> b
mkFocus src ftr (Name (OccName tNm) _) = goFtr ftr id
  where
  goFtr :: forall fs' rs a' b'. FocusFeature fs' src rs a' b' -> (Q [Dec] -> a') -> b'
  goFtr f k = case f of
    f1 :& f2 -> goFtr f1 $ \as -> goFtr f2 $ \bs -> k $ (++) <$> as <*> bs
    MakeNewtype    -> withName tNm $ \mn -> case mn of
      Just nm -> k $ do
        inf <- reify nm
        return []
      _       -> k $ return []
    MakeIso        -> withName tNm $ \mn -> case mn of
      Just nm -> k $ return []
      _       -> k $ return []
    MakeCongruence -> case src of
      TypeFamily -> knownTrue  $ \nm  -> k $ do
        return []
      TypeSig    -> knownFalse $ withNmFn tNm $ \nm -> k $ do
        return []
      Promoted   -> knownFalse $ withNmFn tNm $ \nm -> k $ do
        return []

withNmFn :: String -> (Maybe Name -> a) -> (String -> Maybe String) -> a
withNmFn tNm f nmF = f $ fmap mkName $ nmF tNm

withName :: String -> (Maybe Name -> a) -> Either String (String -> Maybe String) -> a
withName tNm f e = case e of
  Left  nm  -> f $ Just $ mkName nm
  Right nmF -> case nmF tNm of
    Just nm -> f $ Just $ mkName nm
    _       -> f Nothing

class FeatureSet (fs :: [Feature]) where
  featureSet :: p fs -> Set Feature
instance FeatureSet '[] where
  featureSet _ = S.empty
instance FeatureSet fs => FeatureSet (MkNt ': fs) where
  featureSet _ = S.insert MkNt $ featureSet (Proxy :: Proxy fs)
instance FeatureSet fs => FeatureSet (MkIso ': fs) where
  featureSet _ = S.insert MkIso $ featureSet (Proxy :: Proxy fs)
instance FeatureSet fs => FeatureSet (MkCong ': fs) where
  featureSet _ = S.insert MkCong $ featureSet (Proxy :: Proxy fs)

-- Elem {{{

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

class Subset (as :: [k]) (bs :: [k])
instance Subset '[] bs
instance (Elem a bs ~ True, Subset as bs) => Subset (a ': as) bs

type family Elem (a :: k) (as :: [k]) :: Bool where
  Elem a '[]       = False
  Elem a (b ': as) = (a == b) || Elem a as

data List (f :: k -> *) (as :: [k]) where
  Nil  :: List f '[]
  Cons :: f a -> List f as -> List f (a ': as)

{-
elem :: TestBEquality f => f a -> List f as -> IsElem (Elem a as) a as
elem fa as = case as of
  Nil         -> NoElems
  Cons fb as' -> case testBEquality fa fb of
    True_  -> Head
    False_ -> _Tail $ elem fa as'
-}

data IsElem (p :: Bool) (a :: k) (as :: [k]) where
  Head     :: ((a == b) ~ True)  =>                     IsElem True a (b ': as)
  Tail     :: ((a == b) ~ False) => IsElem True a as -> IsElem True a (b ': as)
  NoElems  :: IsElem False a as

_Tail :: ((a == b) ~ False) => IsElem p a as -> IsElem p a (b ': as)
_Tail p = case p of
  Head    -> Tail Head
  Tail t  -> Tail $ Tail t
  NoElems -> NoElems

data When (p :: Bool) (c :: *) (a :: *) where
  KnownTrue   :: c -> When True  c a
  KnownFalse  :: a -> When False c a
  Contingent :: (p ~ True => c) -> (p ~ False => a) -> When p c a

knownTrue :: (c -> b) -> When True c a -> b
knownTrue f w = f $ case w of
  KnownTrue c    -> c
  Contingent c _ -> c

knownFalse :: (a -> b) -> When False c a -> b
knownFalse f w = f $ case w of
  KnownFalse   a -> a
  Contingent _ a -> a

class TestBEquality (s :: *) (t :: *) (a :: k) (b :: k) where
  testBEquality :: s -> t -> B (a == b)

data B (x :: Bool) where
  True_  :: B True
  False_ :: B False

-- }}}

{-
-- Old {{{
data FocusRules = FocusRules
  { _focusNewtype :: String -> Maybe String
  , _focusIso     :: String -> Maybe String
  , _focusCong    :: String -> Maybe String
  , _focusSrcType :: FocusType
  }

data FocusType
  = TyFamFocus
  | DataFocus
  | PromotedFocus
  deriving (Eq,Show)

makeLenses ''FocusRules

defaultRules :: FocusRules
defaultRules = FocusRules
  { _focusNewtype = ntFn
  , _focusIso     = isoFn
  , _focusCong    = congFn
  , _focusSrcType = TyFamFocus
  }
  where
  ntFn, isoFn, congFn :: String -> Maybe String
  ntFn = nameCase $ \cl -> case cl of
    VarId  -> const Nothing
    ConId  -> Just . (++ "_")
    VarSym -> Just . (++ ":")
    ConSym -> Just . (++ ":")
  isoFn = nameCase $ \cl -> case cl of
    VarId  -> const Nothing
    ConId  -> Just . ('_' :)
    VarSym -> Just . (++ ":")
    ConSym -> Just . ('.' :)
  congFn = nameCase $ \cl -> case cl of
    VarId  -> const Nothing
    ConId  -> Just . ("cong" ++)
    VarSym -> const Nothing
    ConSym -> undefined
-- }}}
-}

