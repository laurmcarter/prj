{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Staged.Compile where

import Data.Staged.Class
import Prelude hiding ((<=))
import qualified Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

newtype Compile a = Compile
  { compile :: Q Exp
  }

-- Instances and Ops {{{

instance Num (Compile Int) where
  fromInteger i = Compile [| n |]
    where
    n :: Int
    n = fromInteger i
  x + y    = Compile [| x Prelude.+  y |]
  x * y    = Compile [| x Prelude.*  y |]
  x - y    = Compile [| x Prelude.-  y |]
  abs x    = Compile [| abs x |]
  signum x = Compile [| signum x |]

class Lower t where
  lower :: Q Exp -> t

instance Lower (Compile a) where
  lower = Compile

instance Lift (Compile a) where
  lift (Compile e) = e

instance (Lower a, Lift b) => Lift (a -> b) where
  lift f = do
    v <- newName "x"
    lamE [varP v] $ lift $ f $ lower $ varE v

printCode :: Compile a -> IO ()
printCode c = runQ (compile c) >>= print

prettyCode :: Compile a -> IO ()
prettyCode c = runQ (compile c) >>= print . ppr

-- }}}

instance Sym Compile where
  bool b    = Compile [| b |]
  ----
  lam f     = Compile [| f |]
  app f x   = Compile [| f x |]
  fix f     = Compile [| let x = f x in x |]
  ----
  x <= y    = Compile [| x Prelude.<= y |]
  ----
  if_ t c a = Compile [| if t then c else a |]

cond :: a -> a -> Bool -> a
cond t f b = if b then t else f

{-
  int  i    = Compile [| i |]
  x + y     = Compile [| x Prelude.+  y |]
  x * y     = Compile [| x Prelude.*  y |]
  x - y     = Compile [| x Prelude.-  y |]
-}

