{-# LANGUAGE ViewPatterns #-}

module Text.Quote.Simple.Chunk
  ( Chunk(..)
  , chunkText
  ) where

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char

import Control.Applicative

data Chunk
  = T Text -- ^ quasiquoted text
  | E Text -- ^ antiquoted  expression
  | V Text -- ^ antiquoted  value
  deriving (Eq,Show)

chunkText :: Text -> Either String [Chunk]
chunkText = parseOnly parseChunks

parseChunks :: Parser [Chunk]
parseChunks = fmap concat $ flip manyTill endOfInput $ do
  text <- takeTill (== '$')
  end  <- atEnd
  if end
    then return [T text]
    else do
      char '$'
      next <- anyChar
      case next of
        '(' -> do expr <- parseExpr
                  return [T text, E expr]
        (isIdentInit -> True)
            -> do name <- takeTill $ not . isIdent
                  return [T text, V $ T.cons next name]
        _   ->    return [T $ text `T.snoc` '$' `T.snoc` next]

parseExpr :: Parser Text
parseExpr = T.concat <$> parseExprLevels 0

parseExprLevels :: Int -> Parser [Text]
parseExprLevels = go
  where
  go lvl = do
    expr  <- takeTill isParen
    paren <- anyChar
    let addit = (expr:).(T.singleton paren:)
    case paren of
      ')' | lvl <= 0  -> return    [expr]
          | otherwise -> addit <$> go (lvl-1)
      '('             -> addit <$> go (lvl+1)
      _               -> return    [expr,T.singleton paren]

-- Helpers {{{

isParen :: Char -> Bool
isParen c = c `elem` "()"

isIdentInit :: Char -> Bool
isIdentInit c =
     isAlpha c
  || isDigit c
  || '_' ==  c

isIdent :: Char -> Bool
isIdent c =
     isIdentInit c
  || '\'' ==     c

-- }}}

