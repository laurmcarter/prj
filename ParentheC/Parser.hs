{-# LANGUAGE OverloadedStrings #-}

import qualified Language.Sexp as S
import           Data.Attoparsec.Lazy
import           Data.Attoparsec.Combinator

import           Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C8

import           Control.Applicative
import           Control.Monad
import           Data.Char (isDigit)

main :: IO ()
main = do
  txt <- BS.readFile "sexp.ss"
  case parseSexps txt of
    Left (err,_) -> do putStrLn $ "Error:\n  " ++ err
    Right ss -> mapM_ (C8.putStrLn . pprintS) ss

data Sexp
  = L [Sexp]
  | S ByteString
  | B Bool
  | I Int
  deriving (Eq,Show)

parseSexps :: ByteString -> Either String [Sexp]
parseSexps bs = do
  sexp <- S.parse bs
  case specify sexp of
    Just
  where
  specify :: S.Sexp -> Maybe Sexp
  specify sexp = case sexp of
    S.List ss -> L <$> mapM specify ss
    S.Atom s  -> msum
                   [ B <$> bool s
                   , I <$> integer s
                   , S <$> pure s
                   ]

pprintS :: Sexp -> ByteString
pprintS = S.printHum . toSexp
  where
  toSexp :: Sexp -> S.Sexp
  toSexp (L ss) = S.List $ map toSexp ss
  toSexp (S bs) = S.Atom bs
  toSexp (B b)  = S.Atom $ C8.pack $ if b then "#t" else "#f"
  toSexp (I i)  = S.Atom $ C8.pack $ show i

integer :: Monad m => ByteString -> m Int
integer bs
  | C8.all isDigit bs = case C8.readInt bs of
                          Just (n,_) -> return n
                          _          -> fail "Not an Int"
  | otherwise      = fail "Not an Int"

bool :: Monad m => ByteString -> m Bool
bool bs
  | bs == "#t" = return True
  | bs == "#f" = return False
  | otherwise  = fail "Not a Bool"

