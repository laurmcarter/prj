{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module Data.Interpreter.Format where

import Prelude hiding ((^))
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.List (stripPrefix)

class Format r where
  lit  :: String -> r a a
  int  :: r a (Int  -> a)
  char :: r a (Char -> a)
  (^)  :: r b c -> r a b -> r a c
infixl 5 ^

type Fmt a b = forall r. Format r => r a b

f0 :: Fmt a a
f0 = lit "Hello, world!"

f1 :: Fmt a (Int -> Char -> a)
f1 = lit "an int, " ^ int ^ lit ", and a char, " ^ char ^ lit "!"

newtype FPr a b = FPr
  { sprintFmt :: (String -> a) -> b
  }

instance Format FPr where
  lit s = FPr $ \k -> k s
  int   = FPr $ \k -> k . show
  char  = FPr $ \k -> k . (:[])
  a ^ b = FPr $ \k ->
    sprintFmt a $ \sa ->
    sprintFmt b $ \sb ->
    k $ sa ++ sb

sprintf :: FPr String b -> b
sprintf fmt = sprintFmt fmt id

newtype FSc a b = FSc
  { scanFmt :: (b,String) -> Maybe (a,String)
  }

scanf :: FSc a b -> String -> b -> Maybe (a,String)
scanf f inp b = scanFmt f (b,inp)

instance Format FSc where
  lit     = FSc . scanString
  int     = FSc   scanInt
  char    = FSc   scanChar
  fa ^ fb = FSc $ scanFmt fa >=> scanFmt fb

scanInt :: (Int -> a,String) -> Maybe (a,String)
scanInt (f,inp) = case span isDigit inp of
  (i,inp') | not (null i) -> Just (f $ read i,inp')
  _                       -> Nothing

scanChar :: (Char -> a,String) -> Maybe (a,String)
scanChar (f,inp) = case inp of
 c:inp' -> Just (f c,inp')
 _      -> Nothing

scanString :: String -> (a,String) -> Maybe (a,String)
scanString s (a,inp) = (,) a <$> stripPrefix s inp

