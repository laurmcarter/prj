{-# LANGUAGE ForeignFunctionInterface #-}

module Test where

import System.Environment

foreign export ccall test :: IO ()
foreign export ccall test2 :: IO ()

test :: IO ()
test = putStrLn "Hello, C. Call me anytime."

test2 :: IO ()
test2 = print =<< getArgs

