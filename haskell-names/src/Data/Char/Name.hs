{-# LANGUAGE ViewPatterns #-}

module Data.Char.Name where

import Control.Applicative
import Data.Char

-- Char Predicates {{{

small :: Char -> Bool
small  = mem ['a'..'z'] <||> (== '_')

large :: Char -> Bool
large  = mem ['A'..'Z']

symbol :: Char -> Bool
symbol = mem "!#$%&*+./<=>?@\\^|-~"
  <||> (isSymbol <||> isPunctuation)
  <&&> not . mem "(),;[]`{}_\"'"

idChar :: Char -> Bool
idChar = small <||> large <||> isDigit <||> (== '\'')

symChar :: Char -> Bool
symChar = symbol <||> (== ':')

-- }}}

-- String Predicates {{{

varId :: String -> Bool
varId  = word small idChar

conId :: String -> Bool
conId  = word large idChar

varSym :: String -> Bool
varSym = word symbol symChar

conSym :: String -> Bool
conSym = word (== ':') symChar

-- }}}

-- Case Factoring {{{

nameCase :: (NameClass -> String -> Maybe r) -> String -> Maybe r
nameCase f nm = case nm of
  c:rest
    | small    c , all idChar  rest -> go VarId
    | large    c , all idChar  rest -> go ConId
    | symbol   c , all symChar rest -> go VarSym
    | (== ':') c , all symChar rest -> go ConSym
  _ -> Nothing
  where
  go = flip f nm

data NameClass
  = VarId
  | ConId
  | VarSym
  | ConSym
  deriving (Eq,Ord,Show)

-- }}}

-- Util {{{

mem :: Eq a => [a] -> a -> Bool
mem = flip elem

word :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
word h t as = case as of
  []     -> False
  c : cs -> h c && all t cs

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
infixr 3 <&&>

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
infixr 2 <||>

capital :: String -> String
capital s = case s of
  c : rest -> toUpper c : rest
  _        -> s

-- }}}

