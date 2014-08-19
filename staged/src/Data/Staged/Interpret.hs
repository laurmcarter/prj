{-# LANGUAGE FlexibleInstances #-}

module Data.Staged.Interpret where

import Data.Staged.Class

newtype Eval a = Eval
  { eval :: a }
  deriving (Eq,Ord,Show)
  
instance Sym Eval where
  bool      = Eval
  ----
  lam f     = Eval $ eval . f . Eval
  app f x   = Eval $ eval f $ eval x
  fix f     = Eval $ let x = eval $ f $ Eval x in x
  ----
  x <= y    = Eval $ eval x <=: eval y
  ----
  if_ t c a = Eval $ if eval t then eval c else eval a

instance Num (Eval Int) where
  fromInteger = Eval . fromInteger
  x + y     = Eval $ eval x +:  eval y
  x * y     = Eval $ eval x *:  eval y
  x - y     = Eval $ eval x -:  eval y
  abs       = Eval . abs . eval
  signum    = Eval . signum . eval

