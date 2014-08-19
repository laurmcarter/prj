{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parse where

import Control.Applicative ((<$>),(<*),(<*>),(*>))
import Control.Monad.Morph
import Control.Monad.State
import Control.Monad.Trans.Class
import Text.Parsec hiding (State(..))
import Text.Parsec.Pos
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Indent

type P  = Parser
type IP a = IndentParser String () a

data Stmt
  = Arrow Expr Expr
  | Simple Expr
  deriving (Eq,Show)

stmt :: IP Stmt
stmt = Simple <$> expr

data Expr
  = E String [Expr]
  | I Integer
  | D Double
  deriving (Eq,Show)

expr :: IP Expr
expr = choice
  [ I <$> integer
  , D <$> float
  , E <$> identifier <*> option [] (many1 expr)
  ]

stmts :: IP [Stmt]
stmts = braces $ semiSep stmt

doBlock :: IP [Stmt]
doBlock = withBlock' (symbol "do") stmt

testParse :: IP a -> String
  -> Either ParseError a
testParse p = runIndent ""
  . runParserT p () ""

test :: IP a -> IO (Either ParseError a)
test p = do
  txt <- readFile "test.txt"
  return $ testParse p txt

-- Puns for Funs {{{

P.TokenParser
 { P.identifier
 , P.reserved
 , P.operator
 , P.reservedOp
 , P.charLiteral
 , P.stringLiteral
 , P.natural
 , P.integer
 , P.float
 , P.naturalOrFloat
 , P.decimal
 , P.hexadecimal
 , P.octal
 , P.symbol
 , P.lexeme
 , P.whiteSpace
 , P.parens
 , P.braces
 , P.angles
 , P.brackets
 , P.squares
 , P.semi
 , P.comma
 , P.colon
 , P.dot
 , P.semiSep
 , P.semiSep1
 , P.commaSep
 , P.commaSep1
 } = P.makeTokenParser $ indentHaskell

indentHaskell :: GenLanguageDef String st (State SourcePos)
indentHaskell = P.LanguageDef
  { P.identStart  = letter
  , P.identLetter = alphaNum <|> oneOf "_'"
  , P.opStart     = opLetter'
  , P.opLetter    = opLetter'
  , ..
  }
  where
  opLetter' = oneOf ":!#$%&*+./<=>?@\\^|-~"
  P.LanguageDef
    { P.identStart  = is
    , P.identLetter = il
    , P.opStart     = os
    , P.opLetter    = ol
    , ..
    } = haskellDef


-- }}}

