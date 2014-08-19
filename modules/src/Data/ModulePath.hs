{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.ModulePath where

import Data.Sequence1 (Seq1R)
import qualified Data.Sequence1 as S1

import Control.Applicative
import Control.Lens
import Data.Char
import Data.Sequence (Seq)
import qualified Data.Sequence as Sq
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as R

newtype ModulePath = ModulePath
  { _modulePath :: Seq1R Text
  } deriving (Eq,Ord,Show)

makeIso ''ModulePath

mkModule :: Seq Text -> Text -> ModulePath
mkModule = curry $ view $ from S1._Seq1 . modulePath

fromList :: [Text] -> ModulePath
fromList ts = case Sq.viewr $ Sq.fromList ts of
  ps Sq.:> t -> mkModule ps t
  _ -> error "pathFromList: empty list"

toList :: ModulePath -> [Text]
toList = view $ from modulePath . re S1._FromList

descendPath :: ModulePath -> (Text,Maybe ModulePath)
descendPath = view
    ( from modulePath         -- ModulePath      -> Seq1R Text
    . S1._Seq1 . from S1.htil -- Seq1R Text      -> (Seq Text,Text)
    . alongside id            -- (Seq Text,Text) -> (Text,__________)
       ( pre S1._FromSeq      --                      ... Seq1R Text
       . mapping modulePath   --                      ... ModulePath
       )
    )

basePath :: Lens' ModulePath (Seq Text)
basePath = from modulePath . S1._Seq1Rest

baseName :: Lens' ModulePath Text
baseName = from modulePath . S1._Seq1Focus

parseModule :: ReadP ModulePath
parseModule = do
  ns <- R.sepBy1 ((:) <$> R.satisfy isUpper <*> R.munch1 isAlphaNum) (R.char '.')
  case Sq.viewr $ Sq.fromList $ map T.pack ns of
    ps Sq.:> t -> return $ mkModule ps t
    _ -> R.pfail

toDirectoryPath :: ModulePath -> FilePath
toDirectoryPath =
    joinPath
  . toListOf
    ( from modulePath
    . folded
    . to T.unpack
    )

toFilePath :: ModulePath -> FilePath
toFilePath =
    flip addExtension "hs"
  . toDirectoryPath

toText :: ModulePath -> Text
toText = T.intercalate "." . toList

