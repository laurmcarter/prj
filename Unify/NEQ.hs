{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Unify.NEQ where

import Unify
import Unify.CKanren
import Unify.MKTypes

neqC :: (Unifiable f LV) => Subst (Mu f) -> Goal LV f
neqC p = do
  s <- getSubst
  mapM_ (uncurry unify) p
  s' <- getSubst
  if s == s'
    then fail ("=/=C: " ++ show p)
    else normalizeStore p

normalizeStore :: (Unifiable f LV) => Subst (Mu f) -> Goal LV f
normalizeStore p = do
  c <- getConstraints
  f c []
  where
  f c c' = case c of
    [] -> modifyC $ const (

