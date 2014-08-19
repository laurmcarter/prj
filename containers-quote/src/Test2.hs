{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Static
import Test
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  print $([st| bar M.! "bar" |])
  print $([st| bar M.! "foo" |])

