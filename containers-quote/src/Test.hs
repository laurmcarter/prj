{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Test where

import Data.Char (isSpace)
import Data.Map.Static
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

bar :: Map Text Int
bar = M.fromList
  [ ( "foo" , 1 )
  , ( "bar" , 2 )
  ]


