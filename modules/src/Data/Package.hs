{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Package where

import Control.Applicative
import Control.Lens
import Data.Char
import Data.List (intercalate)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as R

-- Constraints {{{

data Constraints
  = Exact Version
  | Inexact Inexact
  deriving (Eq,Ord,Show)

data Inexact
  = Lower Lower
  | Upper Upper
  | Both Lower Upper
  deriving (Eq,Ord,Show)

data Lower
  = LowerGT Version
  | LowerGE Version
  deriving (Eq,Ord,Show)

data Upper
  = UpperLE Version
  | UpperLT Version
  deriving (Eq,Ord,Show)

-- }}}

data Package = Package
  { _package     :: Text
  , _constraints :: Maybe Constraints
  } deriving (Eq,Ord,Show)

makeLenses ''Package

type Packages = Set Package

toList :: Packages -> [Package]
toList = S.toList

unconstrained :: Text -> Package
unconstrained p = Package p Nothing

addPackage :: Package -> Packages -> Packages
addPackage = S.insert

-- Parsing {{{

parsePackage :: ReadP Package
parsePackage = do
  ns <- R.sepBy1 (R.munch1 isAlphaNum) (R.char '-')
  let pnm = T.pack $ intercalate "-" ns
  cs <- R.option Nothing $ Just <$> parseConstraints
  return Package { _package = pnm , _constraints = cs }

parseConstraints :: ReadP Constraints
parseConstraints = R.choice
  [ Exact <$> parseExact
  , Inexact <$> parseInexact
  ]

parseExact :: ReadP Version
parseExact = R.string "==" *> parseVersion
parseInexact :: ReadP Inexact
parseInexact = R.choice
  [ Both <$> (parseLower <* R.string "&&") <*> parseUpper
  , flip Both <$> (parseUpper <* R.string "&&") <*> parseLower
  , Lower <$> parseLower
  , Upper <$> parseUpper
  ]

parseLower :: ReadP Lower
parseLower  = R.char '>' *> R.choice
  [ LowerGE <$> (R.char '=' *> parseVersion)
  , LowerGT <$> parseVersion
  ]

parseUpper :: ReadP Upper
parseUpper  = R.char '<' *> R.choice
  [ UpperLE <$> (R.char '=' *> parseVersion)
  , UpperLT <$> parseVersion
  ]

-- }}}

-- Rendering {{{

toText :: Package -> Text
toText (Package nm mc) = nm <> maybe "" renderConstraints mc

renderConstraints :: Constraints -> Text
renderConstraints c = case c of
  Exact v   -> "==" <> rVersion v
  Inexact i -> rInexact i
  where
  rVersion = T.pack . showVersion
  rInexact i = case i of
    Lower  l -> rLower l
    Upper  u -> rUpper u
    Both l u -> rLower l <> "&&" <> rUpper u
  rLower l = case l of
    LowerGT v -> ">"  <> rVersion v
    LowerGE v -> ">=" <> rVersion v
  rUpper u = case u of
    UpperLE v -> ">=" <> rVersion v
    UpperLT v -> ">"  <> rVersion v

-- }}}

