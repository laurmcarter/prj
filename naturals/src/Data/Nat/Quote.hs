{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Nat.Quote where

import Data.Nat

import Data.Proxy
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

nat :: QuasiQuoter
nat = QuasiQuoter
  { quoteExp  = mkProxy . natT
  , quotePat  = stub
  , quoteType = natT
  , quoteDec  = stub
  }
  where
  stub = error "unprovided"

pattern BinOp o x y <- UInfixE x (VarE (Name (OccName o) _)) y
pattern Int n <- LitE (IntegerL n)

natT :: String -> Q Type
natT s = case parseExp s of
  Left err -> fail err
  Right e  -> go =<< [| $(return e) |]
  where
  go e = case e of
    BinOp "+" x y -> [t| Add $(go x) $(go y) |]
    BinOp "-" x y -> [t| Sub $(go x) $(go y) |]
    BinOp "*" x y -> [t| Mul $(go x) $(go y) |]
    BinOp "/" x y -> [t| Div $(go x) $(go y) |]
    Int n         -> toNat n
    VarE x        -> return $ VarT x

mkProxy :: Q Type -> Q Exp
mkProxy t = [| Proxy :: Proxy $t |]

toNat :: Integer -> Q Type
toNat = iterateN [t| 'Z |] $ \ty -> [t| 'S $ty |]

iterateN :: a -> (a -> a) -> Integer -> a
iterateN z f 0 = z
iterateN z f n = f $ iterateN z f $ n - 1

