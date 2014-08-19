{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Vec.Quote where

import Data.Vec
import Data.Nat.Quote

import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Quote

vec :: QuasiQuoter
vec = QuasiQuoter
  { quoteExp  = vecE
  , quotePat  = vecP
  , quoteType = vecT
  , quoteDec  = stub
  }
  where
  stub = error "unprovided"

vecE :: String -> Q Exp
vecE s = case parseExp $ brackets s of
  Left err -> fail err
  Right (ListE es) -> go es
  where
  go :: [Exp] -> Q Exp
  go []     = [|Nil|]
  go (e:es) = [| $(return e) :* $(go es) |]

vecT :: String -> Q Type
vecT s = [t| Vec $(natT s) |]

vecP :: String -> Q Pat
vecP s = case parsePat $ brackets s of
  Left err -> fail err
  Right (ListP ps) -> go ps
  where
  go :: [Pat] -> Q Pat
  go []     = [p|Nil|]
  go (p:ps) = [p| $(return p) :* $(go ps) |]

brackets :: String -> String
brackets = ("[" ++) . (++ "]")

