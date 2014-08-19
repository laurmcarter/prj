
module Main where

import Data.Maybe (fromMaybe)
import System.Directory (findExecutable)
import System.Environment (getArgs)
import System.Exit
import System.FilePath.Search

main :: IO ()
main = do
  (pat,mout,mall) <- parseArgs
  patternSearch mkAgCmd pat
    (fromMaybe "/home/kcarter/bin/go" mout)
    (fromMaybe False                  mall)

mkAgCmd :: Pattern -> FilePath -> IO String
mkAgCmd pat dir = do
  ag <- maybe noAg return =<< findExecutable "ag"
  return $ ag ++ " -g'/"++pat++"$' " ++ dir
  where
  noAg = do
    putStrLn "Couldn't find ag executable"
    exitFailure

parseArgs :: IO
  ( Pattern
  -- ^ Search pattern
  , Maybe FilePath
  -- ^ non-default output file for script
  , Maybe Bool
  -- ^ don't prompt for more
  )
parseArgs = do
  go =<< getArgs
  where
  go as = case as of
    ("-p":as') -> do
      (f,mout,_) <- go as'
      return (f,mout,Just True)
    ("-o":o:as') -> do
      (f,_,mall) <- go as'
      return (f,Just o,mall)
    [f] -> return (f,Nothing,Nothing)
    _   -> usage flags >> exitFailure

flags :: Flags
flags =
  [ ( "-o <file>" , "Specify file to output bash script" )
  , ( "-p"        , "Prompt to continue search" )
  , ( "-h"        , "Display this help" )
  ]

