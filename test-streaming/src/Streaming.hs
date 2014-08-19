
module Streaming where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Pipes
import qualified Pipes.ByteString as B
import qualified Pipes.Safe as S
import qualified Pipes.Safe.Prelude as S
import System.IO
import System.Process.Streaming

example1 :: IO (Either String ((),()))
example1 = exitCode show $
  execute prg show $ separate
    (consume "stdout.log")
    (consume "stderr.log")
  where
  consume file = surely . safely . useConsumer
    $ S.withFile file WriteMode B.toHandle
  prg = shell "{ echo ooo ; echo eee 1>&2 ; }"

