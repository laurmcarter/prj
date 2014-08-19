{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Num.Quote
  ( numPrismQ
  , base
  , x, o, b
  ) where

import Control.Lens
import qualified Numeric.Lens as N
import Control.Applicative
import Control.Monad (msum)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Proxy
import Data.Data
import Data.Typeable
import Data.Char (isSpace)
import Data.Foldable (Foldable)
import qualified Data.Foldable as F

x :: QuasiQuoter
x = numPrismQ N.hex

o :: QuasiQuoter
o = numPrismQ N.octal

b :: QuasiQuoter
b = numPrismQ N.binary

base :: Int -> QuasiQuoter
base i = numPrismQ $ N.base i

type NumP = Prism' String Integer

numPrismQ :: NumP -> QuasiQuoter
numPrismQ l = QuasiQuoter
  { quoteExp  = numQ l sigE litE tupE
  , quotePat  = numQ l sigP litP tupP
  , quoteType = bad "Type"
  , quoteDec  = bad "Dec"
  }
  where
  bad = unprovErr "numPrismQ"

numQ :: NumP
  -- ^ Prism to parse String
  -> (Q a -> Q Type -> Q a)
  -- ^ add a type signature
  -> (Lit -> Q a)
  -- ^ make one
  -> ([Q a] -> Q a)
  -- ^ make many
  -> String -> Q a
numQ l sig one many o@(words . trim -> ss) =
  case ss of
    []  -> bad
    [s] -> go1 s
    _   -> many $ map go1 ss
  where
  go1 = maybe bad
    ( flip sig (conT ''Integer)
    . one
    . IntegerL
    ) . preview l
  bad = parseErr "numExp" o

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

unprovErr :: String -> String -> a
unprovErr f s = error $ f ++ ": QuasiQuoter for " ++ s ++ " unprovided"

parseErr :: String -> String -> a
parseErr f s = error $ f ++ ": No parse from string " ++ show s

