{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Sample where

import Control.Lens

import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit

data Sample = Sample
  { _install :: Bool
  , _message :: String
  } deriving (Eq,Show,Data,Typeable)

makeLenses ''Sample

sample1 :: Sample
sample1 = Sample
  { _install = def &= help "Should install"
  , _message = "hello world" &= help "Message to print" &= opt "default non-default message"
  } &= summary "Sample v1"

{-
sample2 :: Mode Sample
sample2 = mode "sample2" sample1 "Sample2 help message" (flagArg (upd "foo") "FOO")
  [
  ]
  where
  upd msg x v = Right $ 
-}

