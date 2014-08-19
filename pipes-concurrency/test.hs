
import Control.Concurrent
import Control.Monad
import Control.Proxy

import Control.Concurrent.Async
import Control.Proxy.Concurrent

worker :: (Proxy p, Show a) => Int -> () -> Consumer p a IO r
worker i () = runIdentityP $ forever $ do
  a <- request ()
  lift $ threadDelay 1000000
  lift $ putStrLn $ "Worker #" ++ show i ++ ": Processed " ++ show a

user :: (Proxy p) => () -> Producer p String IO ()
user = stdinS >-> takeWhileD (/= "quit")

main = do
  (input,output) <- spawn Unbounded
  as <- forM [1..3] $ \i -> async $ do
    runProxy $ recvS output >-> worker i
    performGC
  a  <- async $ do
    runProxy $ user >-> sendD input
    performGC
  mapM_ wait (a:as)

