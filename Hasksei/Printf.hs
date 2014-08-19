{-# LANGUAGE TemplateHaskell #-}

module Printf where

import Language.Haskell.TH

data Format = D | S | L String deriving Show

-- a poor man's tokenizer
tokenize :: String -> [Format]
tokenize s = case s of
  []      -> []
  '%':c:s' | c /= '%'
          -> case c of
    'd' -> D : tokenize s'
    's' -> S : tokenize s'
    _   -> error ("unrecognized substitution: %" ++ [c])
  c:s'    -> let fs = tokenize s' in case fs of
    L s : fs' -> L (c:s) : fs'
    _         -> L [c] : fs

-- generate argument list for the function
args :: [Format] -> [PatQ]
args fmt = concatMap (\(f,n) -> case f of
  L _ -> []
  _   -> [varP n]) $ zip fmt names
  where names = [ mkName $ 'x' : show i | i <- [0..] ]

-- generate body of the function
body :: [Format] -> ExpQ
body fmt = foldr (\ e e' -> infixApp e [| (>>) |] e') [| return () |] exps
  where
  exps = [ case f of
    L s -> appE [| putStr |] $ stringE s
    D   -> appE [| putStr |] $ appE [| show |] (varE n)
    S   -> appE [| putStr |] $ varE n
    | (f,n) <- zip fmt names ]
  names = [ mkName $ 'x' : show i | i <- [0..] ]

-- glue the argument list and body together into a lambda
-- this is what gets spliced into the haskell code at the call
-- site of "printf"
printf :: String -> Q Exp
printf format = lamE (args fmt) (body fmt)
  where fmt = tokenize format

