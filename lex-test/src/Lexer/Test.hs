{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lexer.Test where

import Control.Monad (forM_)
import Language.Haskell.Lexer
import Text.Heredoc
import Text.Show.Pretty

test1 :: String
test1 =
  [str|foo :: Maybe Int
      |foo = Just 1
      |]

test2 :: String
test2 =
  [str|do a <- foo 1
      |   b <- do
      |     bar 2
      |     a >< quux
      |     baz 3
      |   return b
      |]

----

runTests :: IO ()
runTests = forM_ [test1,test2]
  $ \t -> putStrLn
    $ unlines
      [ "Input:"
      , t
      , ""
      , "Output:"
      , ppShow $ lexerPass1 t
      , ""
      ]
    

----

pPrint :: Show a => a -> IO ()
pPrint = putStrLn . ppShow

