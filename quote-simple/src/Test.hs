{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Test where

import Prelude
import Text.Quote.Simple.Example
import Control.IxMonad.Trans.State

foo :: IxState () String (Maybe String)
foo = [ido|
  put 3
  modify odd
  modify show
  modify ("Test: " ++)
  return $(return "Success!")
  |]

bar :: Maybe Int
bar = return 3

