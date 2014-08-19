{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Type.Inference where

data Context a
  = MT
  | BindMeta (Meta a)            Kind   (Context a)
  | InstMeta (Meta a) (Type   a) Kind   (Context a)
  | BindVar  (Var  a) (Schema a)        (Context a)
  | Locality                            (Context a)
  deriving (Eq,Show)

data Meta a = M a deriving (Eq,Show)
data Var  a = V a deriving (Eq,Show)

data Type a
  = TVar (Meta a)
  | TArr (Type a) (Type a)
  deriving (Eq,Show)

data Kind
  = Star
  deriving (Eq,Show)

data Schema a
  = Forall (Meta a) (Schema a)
  | T      (Type a)
  deriving (Eq,Show)

data (Stmt a)
  = Valid
  | Typed  (Term   a) (Schema a)
  | Kinded (Schema a) Kind
  | Equal  (Type   a) (Type   a) Kind
  | Inst   (Schema a) (Schema a)
  | Conj   (Stmt   a) (Stmt   a)
  deriving (Eq,Show)

data Term a
  = Var (Var a)
  | Lam (Var  a) (Term a)
  | App (Term a) (Term a)
  | Let (Var  a) (Term a) (Term a)
  deriving (Eq,Show)

-- class Holds (s :: Stmt k) (c :: Context k) (r :: Bool) | s c -> r

type family ValidCxt (c :: Context k) :: Bool where
  ValidCxt MT                 = True
  ValidCxt (BindMeta a k c)   = Fresh a c && ValidCxt c
  ValidCxt (InstMeta a t k c) = Fresh a c && WellKinded (T t) k c
  ValidCxt (BindVar  x s c)   = Fresh x c && WellKinded s     Star c
  ValidCxt (Locality c)       = ValidCxt c
  ValidCxt c = False

type family WellKinded (s :: Schema x) (k :: Kind) (c :: Context x) :: Bool where
  WellKinded (T (TVar a))      k c = Member_ (BindMeta a k) c && ValidCxt c
  WellKinded (T (TArr t v)) Star c = WellKinded (T t) Star c && WellKinded (T v) Star c
  WellKinded (Forall a s)      k c = WellKinded s k (BindMeta a k c)
  WellKinded s k c = False

type family TypesEqual (t :: Type x) (v :: Type x) (k :: Kind) (c :: Context x) :: Bool where
  TypesEqual t t k c = WellKinded (T t) k c
  TypesEqual (TArr t v) (TArr t' v') Star c = TypesEqual t t' Star c && TypesEqual v v' Star c
  TypesEqual (TVar a) t k c = ValidCxt c && Member_ (InstMeta a t k) c
  TypesEqual t v k c = False

{-
-- Equal {{{

instance Holds (Kinded (T t) k) c
  => Holds (Equal t t k) c

instance Holds (Equal t v k) c
  => Holds (Equal v t k) c

instance
  ( Holds (Equal t0 t1 k) c
  , Holds (Equal t1 t2 k) c
  ) => Holds (Equal t0 t2 k) c

-- }}}
-}

-- Member {{{

type family Member_ (a :: Context k -> Context k) (c :: Context k) :: Bool where
  Member_ f MT    = False
  Member_ f (f c) = True
  Member_ f (g c) = Member_ f c

type family Member (a :: l) (k :: Kind) (c :: Context x) :: Bool where
  Member (V a) k c = MemberV a k c
  Member (M a) k c = MemberM a k c

type family MemberM (a :: x) (k :: Kind) (c :: Context x) :: Bool where
  MemberM a k MT = False

  MemberM a k (BindMeta (M a) k c) = True
  MemberM a k (BindMeta (M a) l c) = False
  MemberM a k (BindMeta (M b) l c) = MemberM a k c

  MemberM a k (InstMeta (M a) t k c) = True
  MemberM a k (InstMeta (M a) t l c) = False
  MemberM a k (InstMeta (M b) t l c) = MemberM a k c

  MemberM a k (BindVar v s c) = MemberM a k c

  MemberM a k (Locality c) = MemberM a k c

type family MemberV (a :: x) (k :: Kind) (c :: Context x) :: Bool
{-
  MemberV a k MT = False

  MemberV a k (BindMeta (M a) k c) = True
  MemberV a k (BindMeta (M a) l c) = False
  MemberV a k (BindMeta (M b) l c) = MemberV a k c

  MemberV a k (InstMeta (M a) t k c) = True
  MemberV a k (InstMeta (M a) t l c) = False
  MemberV a k (InstMeta (M b) t l c) = MemberV a k c

  MemberV a k (BindVar v s c) = MemberV a k c

  MemberV a k (Locality c) = MemberV a k c
-}

-- }}}

-- Fresh {{{

type family Fresh (x :: l) (c :: Context k) :: Bool where
  Fresh (V x) c = FreshV x c
  Fresh (M x) c = FreshM x c

type family FreshM (x :: k) (c :: Context k) :: Bool where
  FreshM x MT = True

  FreshM x (BindMeta (M x) k c) = False
  FreshM x (BindMeta (M y) k c) = FreshM x c

  FreshM x (InstMeta (M x) t k c) = False
  FreshM x (InstMeta (M y) t k c) = FreshM x c

  FreshM x (BindVar v s c) = FreshM x c

  FreshM x (Locality c) = FreshM x c

type family FreshV (x :: k) (c :: Context k) :: Bool where
  FreshV x MT = True

  FreshV x (BindMeta m k c) = FreshV x c

  FreshV x (InstMeta m t k c) = FreshV x c

  FreshV x (BindVar (V x) s c) = False
  FreshV x (BindVar (V y) s c) = FreshV x c

  FreshV x (Locality c) = FreshV x c

-- }}}

-- Bools {{{

type family (x :: Bool) && (y :: Bool) :: Bool where
  False && y     = False
  True  && False = False
  True  && True  = True

type family (x :: Bool) || (y :: Bool) :: Bool where
  True  || y     = True
  False || True  = True
  False || False = False

type family Not (x :: Bool) :: Bool where
  Not True  = False
  Not False = True

-- }}}

