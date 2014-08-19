{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.DSL.Render where

import Language.DSL
import Language.Haskell.TH.Syntax (Name(..))
import Control.Applicative
import Data.List (intersperse)

type SP = Int -> ShowS

compose :: [a -> a] -> a -> a
compose = foldr (.) id

sep :: String -> [ShowS] -> ShowS
sep s = compose . intersperse (showString s)

precs :: Int -> [SP] -> [ShowS]
precs d = (<*> pure d)

ppr :: SP -> IO ()
ppr f = putStrLn $ f 0 ""

-- D {{{

instance D ShowS where
  type TypeD ShowS = SP
  type PatD  ShowS = SP
  type ExpD  ShowS = SP
  fun nm ps e =
      shows nm
    . sep " "
      (precs 10 ps)
    . showString " = "
    . e 0
  val p e     =
      p 10
    . showString " = "
    . e 0
  sigD nm t   =
      shows nm
    . showString " :: "
    . t 0

-- }}}

-- T K C {{{

instance T SP where
  type KindT SP = SP
  type CxtT  SP = SP
  type LitT  SP = SP
  forallT vs cs t d =
      showParen (d > 0)
    $ showString "forall "
    . sep " " (map showTV vs)
    . showString ". "
    . ( showParen cxt
      $ sep ", " $ precs 0 cs
      )
    . showString (if cxt then " => " else "")
    . t 1
    where
    cxt = not $ null cs
    showTV :: (Name,Maybe SP) -> ShowS
    showTV (nm,mk) = case mk of
      Just k -> showParen True
        $ shows nm
        . showString " :: "
        . k 0
      _      -> shows nm
  appT a b        d = showParen (d > 10)
    $ a 1
    . showChar ' '
    . b 11
  sigT t k        d = showParen (d > 0)
    $ t 1
    . showString " :: "
    . k 1
  varT nm         _ = shows nm
  conT nm         _ = shows nm
  arrowT          _ = showString "(->)"
  litT l          d = l d

instance K SP where
  forallK ns k d =
      showParen (d > 0)
    $ showString "forall "
    . sep " " (map shows ns)
    . showString ". "
    . k 1
  varK nm      _ = shows nm
  conK nm      _ = shows nm
  arrowK a b   d = showParen (d > 4)
    $ a 5
    . showString " -> "
    . b 4
  starK        _ = showString "*"

instance C SP where
  type TypeC SP = SP
  equalP a b   d = showParen (d > 0)
    $ a 1
    . showString " ~ "
    . b 1
  classP nm ts d = showParen (d > 0)
    $ shows nm
    . showChar ' '
    . sep " " (precs 1 ts)

-- }}}

-- P E {{{

instance P SP where
  type LitP SP = SP
  litP l     d = l d
  varP nm    _ = shows nm
  conP nm ps _ = shows nm
  wildP      _ = showString "_"

instance E SP where
  type TypeE SP = SP
  type PatE  SP = SP
  type LitE  SP = SP
  varE nm    _ = shows nm
  conE nm    _ = shows nm
  litE l     d = l d
  appE e1 e2 d = showParen (d > 10)
    $ e1 11
    . showChar ' '
    . e2 11
  lam ps e   d = showParen (d > 10)
    $ showString "\\"
    . sep " " (precs 11 ps)
    . showString " -> "
    . e 0
  case_ e ms d = showParen (d > 10)
    $ showString "case "
    . e 0
    . showString " of {"
    . sep "; " (map br ms)
    . showChar '}'
    where
    br (p,e) =
        p 0
      . showString " -> "
      . e 0
  sigE e t   d = showParen (d > 0)
    $ e 0
    . showString " :: "
    . t 0

-- }}}

-- L {{{

instance TLCommon SP where
  intTL    n _ = shows n
  stringTL s _ = shows s

instance LNum SP where
  integerL  n _ = shows n
  rationalL n _ = shows n

instance LString SP where
  charL   c _ = shows c
  stringL s _ = shows s

-- }}}

