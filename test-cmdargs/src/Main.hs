
module Main where

import Sample
import System.Console.CmdArgs

main :: IO ()
main = print =<< cmdArgs sample1

