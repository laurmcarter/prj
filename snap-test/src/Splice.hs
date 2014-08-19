{-# LANGUAGE OverloadedStrings #-}

module Splice where

import Heist
import Heist.Interpreted
import Snap
import qualified Data.Text as T
import qualified Text.XmlHtml as X

fooSplice :: MonadSnap m => Splice m
fooSplice = do
  node <- getParamNode
  let nodes = X.childNodes node
  return [X.Element "ul" [] nodes]

barSplice :: MonadSnap m => Splice m
barSplice = do
  node <- getParamNode
  let nodes = X.childNodes node
  return [X.Element "li" [] [X.Element "a" [("href","#")] nodes]]

mySplices :: MonadSnap m => Splices (Splice m)
mySplices = do
  "foo" ## fooSplice
  "bar" ## barSplice

