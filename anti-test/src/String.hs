{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module String where

import Control.Applicative
import Control.Monad ((>=>))
import Data.Data
import Data.Typeable

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.AntiQuoter
import Language.Haskell.Meta.Parse (parseExp,parsePat)
import Text.Trifecta

s :: QuasiQuoter
s = QuasiQuoter
  { quoteExp  = sExp
  , quotePat  = stub
  , quoteType = stub
  , quoteDec  = stub
  }
  where
  stub = undefined "stub"



sExp :: String -> ExpQ
sExp s = undefined





{-
-- old {{{

qExp :: Int -> String -> (Quote,String)
qExp _ "" = (EmptyQ,"")
qExp lvl s = case breakAny preds s of
  -- start of anti
  (s1,'$':'(':rest) -> (Quote s1 a q,rest')
    where
    (a,s2)    = aExp lvl 0 rest
    (q,rest') = qExp lvl s2
  -- end of quote
  (s1,'|':']':rest)
    | lvl > 0 -> (Quote s1 EmptyA EmptyQ,rest)
    | True    -> error "bad quote"
  -- done
  (s1,"") -> (Quote s1 EmptyA EmptyQ,"")
  -- oops
  (s1,s2) -> (Quote (s1++s2) EmptyA EmptyQ,"")
  where
  preds =
    [ (== '$')
    , (== '|')
    ]

aExp :: Int -> Int -> String -> (Anti,String)
aExp _ _ "" = (EmptyA,"")
aExp lvl p s = case breakAny preds s of
  (s1,'[':'|':rest) -> (Anti s1 q a,rest')
    where
    (q,s2)    = qExp (lvl+1) rest
    (a,rest') = aExp lvl p s2
  (s1,'(':rest) -> case a of
    EmptyA       -> (Anti s1 EmptyQ EmptyA,rest')
    Anti s2 q a' -> (Anti (s1++s2) q a',rest')
    where
    (a,rest') = aExp lvl (p+1) rest
  (s1,')':rest)
    | lvl > 0 -> case aExp lvl (p-1) rest of
        (EmptyA      ,rest') -> (Anti s1 EmptyQ EmptyA,rest')
        (Anti s2 q a',rest') -> (Anti (s1++s2) q a'   ,rest')
    | True -> (Anti s1 EmptyQ EmptyA,rest)
  (s1,s2) -> (Anti (s1++s2) EmptyQ EmptyA,"")
  where
  preds =
    [ (== '(')
    , (== ')')
    , (== '[')
    ]

breakAny :: [a -> Bool] -> [a] -> ([a],[a])
breakAny ps = break $ or . (ps <*>) . pure

data Quote
  = Quote String Anti Quote
  | EmptyQ
  deriving (Eq,Show)

data Anti
  = Anti String Quote Anti
  | EmptyA
  deriving (Eq,Show)

{-
sExp = qt [] >=> format
  where
  qt :: [Int] -> String -> Q (Fmt Exp)
  qt lvls s = case s of
    []               -> finish lvls
    '$' : '(' : rest -> aq (0 : lvls) rest
    '$' : rest       -> undefined
    '|' : ']' : rest
      | Just (switch,lvls') <- pop lvls
      -> if switch then aq lvls' rest else fail "Unbalanced parentheses"
      | True
      -> fail "misplaced brackets"
    c : rest -> do
      fmt <- qt lvls rest
      return $ Right c : fmt

  ----
  aq :: [Int] -> String -> Q (Fmt Exp)
  aq lvls = next 0

{-
    [] -> finish lvls
    ')' : rest
      | Just (switch,lvls') <- pop lvls
      -> (if switch then qt else aq) lvls' rest
      | True
      -> badStack
    '[' : '|' : rest -> qt (0:lvls) rest
-}

    where
    next i s = case break (or . (preds <*>) . pure) s of
      (s','('    :rest) -> 
      (s','[':'|':rest) -> _
      (s','|':']':rest) -> 
      (s',')'    :rest) -> 
      (s',_           ) ->
    preds =
      [ (==) ')'
      , (==) '['
      , (==) '|'
      , (==) '('
      ]

  ----
  badStack :: Q a
  badStack = fail "Malformed quasi/antiquote stack"
  ----
  finish :: [Int] -> Q [a]
  finish [] = return []
  finish _  = fail "Unbalanced parentheses"
  ----
  pop :: [Int] -> Maybe (Bool,[Int])
  pop lvls = case lvls of
    0:ls -> Just (True,ls)
    i:ls -> Just (False,(i-1):ls)
    _    -> Nothing
-}

type Fmt a = [Either a Char]

format :: Fmt a -> Q a
format = undefined


sPat :: String -> PatQ
sPat = undefined

-- }}}
-}

