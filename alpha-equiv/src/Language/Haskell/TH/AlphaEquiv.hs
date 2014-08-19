{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Language.Haskell.TH.AlphaEquiv where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Monoid

data EquivError
  = BadKinds Name Kind Kind
  | BadTypes Type Type
  | BadEq String String
  deriving (Eq,Show)

newtype AE a = AE
  { unAE :: ExceptT EquivError (Writer (Names,Kinds)) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadError  EquivError
  , MonadWriter (Names,Kinds)
  )

type Names = Map Name Name
type Kinds = Map Name Kind

class AlphaEquiv t where
  (=?=) :: t -> t -> AE ()
infixr 5 =?=

instance AlphaEquiv Type where
  x =?= y = case (x,y) of
    (ForallT v1 c1 t1  , ForallT v2 c2 t2)  -> undefined
    (AppT f1 x1        , AppT f2 x2)        -> f1 =?= f2 >> x1 =?= x2
    (SigT t1 k1        , SigT t2 k2)        -> t1 =?= t2 >> k1 =?= k2
    (VarT n1           , VarT n2)           -> n1 =?= n2
    (ConT n1           , ConT n2)           -> n1 =?= n2
    (PromotedT n1      , PromotedT n2)      -> n1 =?= n2
    (TupleT i1         , TupleT i2)         -> i1 ?~ i2
    (UnboxedTupleT i1  , UnboxedTupleT i2)  -> i1 ?~ i2
    (ArrowT            , ArrowT)            -> triv
    (ListT             , ListT)             -> triv
    (PromotedTupleT i1 , PromotedTupleT i2) -> i1 ?~ i2
    (PromotedNilT      , PromotedNilT)      -> triv
    (PromotedConsT     , PromotedConsT)     -> triv
    (StarT             , StarT)             -> triv
    (ConstraintT       , ConstraintT)       -> triv
    (LitT l1           , LitT l2)           -> l1 ?~ l2
    _ -> throwError $ BadTypes x y

instance AlphaEquiv Name where
  x =?= y = tell (M.singleton x y,mempty)

{-
instance AlphaEquiv Pred where
  x =?= y = case (x,y) of
    (ClassP n1 t1 , ClassP n2 t2) -> undefined
    (EqualP a1 b1 , EqualP a2 b2) -> undefined
    _ -> Nothing
-}

newtype Subst a b = Subst
  { subst :: Map a b
  }

walkOf :: ASetter s t a b -> 

extNames :: Name -> Name -> AE ()
extNames x y = tell (M.singleton x y,mempty)

extKind :: Name -> Kind -> AE ()
extKind x k = tell (mempty,M.singleton x k)

triv :: AE ()
triv = return ()

(?~) :: (Eq a, Show a) => a -> a -> AE ()
x ?~ y
  | x == y    = triv
  | otherwise = throwError $ BadEq (show x) (show y)
infixr 5 ?~

