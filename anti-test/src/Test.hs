{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.AntiQuoter
import Data.Data
import Data.Typeable

import Control.Applicative
import Control.Lens
import Control.Monad (guard)
import Control.Monad.Cont
import Control.Monad.State
import Data.List
import Data.Monoid
import Text.Trifecta
import qualified Data.ByteString.Char8 as BS

import Debug.Trace

unlns :: [String] -> String
unlns = intercalate "\n"

data Expr
  = VarE Var
  | Int Int
  | Lam Var Expr
  | App Expr Expr
  | AntiExpr String
  deriving (Eq,Show,Data,Typeable)

data Var
  = Var String
  | AntiVar String
  deriving (Eq,Show,Data,Typeable)

e :: QuasiQuoter
e = mkQuasiQuoter
  quoteExpr
  antiExprE
  antiExprP

type Parse m = (DeltaParsing m, Applicative m, Monad m)

quoteExpr :: String -> Q Expr
quoteExpr = undefined -- \case

antiExprE :: AntiQuoter Exp
antiExprE = \case
  Cast (AntiExpr s) -> mkVar s
  Cast (AntiVar  s) -> mkVar s
  _                           -> Nothing
  where
  mkVar :: String -> Maybe (Q Exp)
  mkVar = Just . varE . mkName

antiExprP :: AntiQuoter Pat
antiExprP = \case
  Cast (AntiExpr s) -> mkVar s
  Cast (AntiVar  s) -> mkVar s
  _                           -> Nothing
  where
  mkVar :: String -> Maybe (Q Pat)
  mkVar = Just . varP . mkName

pattern Cast p <- (cast -> Just p)
pattern And a b = Just (a,b)

(.$) :: (a -> b) -> a -> b
f .$ x = f x
infixr 4 .$

(.&) :: a -> (a -> b) -> b
x .& f = f x
infixl 4 .&

run :: Parser a -> String -> Result a
run p = parseString p mempty

-- Chunks {{{

data Chunks s a
  = Final a
  | Chunk a s (Chunks s a)
  deriving (Eq,Show)

makePrisms ''Chunks

_First :: Traversal' (Chunks s a) a
_First = _Final `failing` _Chunk._1

splitAll :: (a -> Maybe s) -> [a] -> Chunks s [a]
splitAll _ [] = Final []
splitAll f (a:as) = case f a of
  Just s -> Chunk [] s $ splitAll f as
  _      -> splitAll f as &
              _First %~ (a:)

isParen :: Char -> Maybe Char
isParen c = c <$ guard (c `elem` "()[]")

type Unfolder a s b = a -> Maybe (Either s b,a)
type UFResult   s b =             Either s b

mkSep   :: s -> UFResult s b
mkSep   = Left
mkChunk :: b -> UFResult s b
mkChunk = Right

data AQDelim
  = AntiL
  | ParenL
  | ParenR
  deriving (Eq,Show)

aqSplit :: Unfolder String AQDelim String
aqSplit = \case
  (stripPrefix "$(" -> Just rest) -> Just (mkSep AntiL  , rest)
  (stripPrefix ")"  -> Just rest) -> Just (mkSep ParenR , rest)
  (stripPrefix "("  -> Just rest) -> Just (mkSep ParenL , rest)
  (c:rest)                        -> Just (mkChunk [c]  , rest)
  _                               -> Nothing

chunks :: Monoid b => Unfolder a s b -> a -> Chunks s b
chunks f = go
  where
  go a = case f a of
    -- there's more
    Just (sb,go -> rest) -> case sb of
      -- found a separator
      Left  s -> Chunk mempty s rest
      -- found a chunk
      Right b -> rest & _First %~ (b <>)
    -- it's done
    _         -> Final mempty

chunkAQ :: String -> Chunks AQDelim String
chunkAQ = chunks aqSplit

ppChunks :: (Show a,Show s) => Chunks s a -> IO ()
ppChunks = putStrLn . unlns . go
  where
  go = \case
    Final a     -> [show a]
    Chunk a s c ->
      [ show a
      , "  " ++ show s
      ] ++ go c

findDelim :: Show s => Finder s b -> Chunks s a -> [(Chunks s a,b,Chunks s a)]
findDelim pr = \case
  Final _        -> []
  Chunk a s rest -> case pr s of
    Just b -> (Final a,b,rest) : rest'
    _      -> rest'
    where
    rest' = map (_1 %~ Chunk a s) $ findDelim pr rest

snocChunk :: s -> a -> Chunks s a -> Chunks s a
snocChunk s a = \case
  Final b -> Chunk b s $ Final a
  Chunk b t rest -> Chunk b t $ snocChunk s a rest

reverseChunks :: Chunks s a -> Chunks s a
reverseChunks = \case
  Final a        -> Final a
  Chunk a s rest -> snocChunk s a $ reverseChunks rest

findDelimFromEnd :: Show s => Finder s b -> Chunks s a -> [(Chunks s a,b,Chunks s a)]
findDelimFromEnd pr = map reverseSplit . findDelim pr . reverseChunks
  where
  reverseSplit (c1,b,c2) = (reverseChunks c2,b,reverseChunks c1)

type Balancer s b = s -> s -> Maybe b
type Finder   s b =      s -> Maybe b

-- }}}

data Balanced b a
  = Leaf a
  | Branch a b (Balanced b a) (Balanced b a)
  deriving (Eq,Show)

makePrisms ''Balanced

data AQP
  = Anti_
  | Paren_
  deriving (Eq,Show)

balance :: Show s => Balancer s b -> Chunks s a -> [Balanced b a]
balance pr = \case
  Final a     -> return $ Leaf a
  Chunk a s c -> do
    (c1,b,c2) <- findDelimFromEnd (pr s) c
    l <- balance pr c1
    r <- balance pr c2
    return $ Branch a b l r

aqpBalance :: Balancer AQDelim AQP
aqpBalance = matches
  [ ( AntiL  , ParenR , Anti_  )
  , ( ParenL , ParenR , Paren_ )
  ]

balanceAQP :: Chunks AQDelim String -> [Balanced AQP String]
balanceAQP = balance aqpBalance

parseBalanced :: (Show s, Show b, Show c, Monoid c)
  => Unfolder a s c -> Balancer s b -> a -> [Balanced b c]
parseBalanced ch bl = balance bl . chunks ch

parseAQP :: String -> [Balanced AQP String]
parseAQP = parseBalanced aqSplit aqpBalance

instance Monoid a => Monoid (Balanced b a) where
  mempty                   = Leaf mempty
  mappend (Leaf a)         = _Leftmost %~ (a <>)
  mappend (Branch a b l r) = Branch a b l . (r <>)

flatten :: Monoid r => (b -> (r,r)) -> (a -> r) -> Balanced b a -> r
flatten fb fa = go
  where
  go = \case
    Leaf a -> fa a
    Branch a b l r -> fa a <> o <> go l <> c <> go r
      where
      (o,c) = fb b

compress :: Monoid a => (b -> Either (a,a) c) -> Balanced b a -> Balanced c a
compress f = go
  where
  go = \case
    Leaf a -> Leaf a
    Branch a b (go -> l) (go -> r) -> case f b of
      Left (o,c) -> Leaf (a <> o) <> (l & _Rightmost <>~ c) <>  r
      Right b'   -> Branch a b' l r

compress' :: Monoid a => (b -> Maybe (a,a)) -> Balanced b a -> Balanced b a
compress' f = compress g
  where
  g b = case f b of
    Just p -> Left p
    _      -> Right b

flattenAQ :: Balanced AQP String -> Balanced () String
flattenAQ = compress $ \case
  Paren_ -> Left ("(",")")
  Anti_  -> Right ()

_Leftmost :: Traversal' (Balanced b a) a
_Leftmost = _Leaf `failing` _Branch._1

_Rightmost :: Traversal' (Balanced b a) a
_Rightmost = _Leaf `failing` _Branch._4._Rightmost

ppBal :: (Show a,Show b) => (b -> (String,String)) -> Balanced b a -> IO ()
ppBal f = putStrLn . go 0
  where
  go i = \case
    Leaf a -> indent i $ show a
    Branch a b sub rest -> unlns
      [ indent i $ show a
      , indent i o
      , go (i+1) sub
      , indent i c
      , go i rest
      ]
      where
      (o,c) = f b
  indent :: Int -> String -> String
  indent i s
    | i <= 0 = s
    | True   = indent (i - 1) $ "  " ++ s

parseTestAQ :: String -> IO ()
parseTestAQ s = do
  putStrLn "Possible parses:\n"
  sequence_
    $ intersperse .$ putStrLn "\n***\n"
    $ map         .$ ppBal fmt . flattenAQ
    $ parseAQP s
  putStrLn ""
  where
  fmt _ = ("$(",")")

matches :: Eq a => [(a,a,b)] -> a -> a -> Maybe b
matches ms k a = do
  (_,a',b) <- find (views _1 (== k)) ms
  guard $ a == a'
  return b

