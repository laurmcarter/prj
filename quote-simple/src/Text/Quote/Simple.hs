{-# LANGUAGE OverloadedStrings #-}

module Text.Quote.Simple where

import Text.Quote.Simple.Chunk

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.Meta

import Control.Arrow (left)
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (Foldable)
import qualified Data.Foldable as F

antiChunks ::
     (Text -> Q a)
  -> (Text -> Q a)
  ->  String -> Q [Either (Q a) Text]
antiChunks aqV aqE = chunkString >=> return . map go
  where
  go c = case c of
    T t -> Right       t
    E t -> Left  $ aqE t
    V t -> Left  $ aqV t

quoteEmbedded :: (Text -> Text) -> (ExpQ -> ExpQ) -> String -> ExpQ
quoteEmbedded tf ef =
      expChunks
  >=> return . map (left ef)
  >=> embedAntiquoted
  >=> \(t,bs) -> bindAntiquoted (tf t) bs

bindAntiquoted :: Text -> [(Name,ExpQ)] -> ExpQ
bindAntiquoted body bs = case bs of
  [] -> parseText body
  _  -> letE (map mkBind bs) $ parseText body
  where
  parseText    = handleM . parseExp . T.unpack
  mkBind (n,e) = funD n [ clause [] (normalB e) [] ]

embedAntiquoted :: [Either ExpQ Text] -> Q (Text,[(Name,ExpQ)])
embedAntiquoted = runWriterT . F.foldrM go ""
  where
  go c rest = case c of
    Left  e -> do x <- lift $ newName "embedded"
                  let x' = show x
                  tell [(mkName x',e)]
                  return $ T.pack x' <> rest
    Right t -> return $ t <> rest

-- Useful Quoters --------------------------------------------------------------

-- Simple ------------------------------

expChunks :: String -> Q [Either ExpQ Text]
expChunks = antiChunks
  (varE . textName)
  (wrapParser parseExp)

patChunks :: String -> Q [Either PatQ Text]
patChunks = antiChunks
  (varP . textName)
  (wrapParser parsePat)

typeChunks :: String -> Q [Either TypeQ Text]
typeChunks = antiChunks
  (varT . textName)
  (wrapParser parseType)

decsChunks :: String -> Q [Either DecsQ Text]
decsChunks = antiChunks
  (const $ fail "Cannot splice Name into Decs")
  (wrapParser parseDecs)

-- Simplest ----------------------------

quoteChunks ::
     ([Chunk] -> Q a)
  ->  String  -> Q a
quoteChunks f = chunkString >=> f

-- Helpers ---------------------------------------------------------------------

wrapParser :: (String -> Either String a) -> Text -> Q a
wrapParser f = handleM . f . T.unpack

textName :: Text -> Name
textName = mkName . T.unpack

handleM :: Monad m => Either String a -> m a
handleM = either fail return

chunkString :: Monad m => String -> m [Chunk]
chunkString = handleM . chunkText . T.pack

