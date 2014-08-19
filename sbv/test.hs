
import Data.SBV

main :: IO ()
main = compileToC (Just "out") "test" $ do
  x <- cgInput "x"
  cgSetDriverValues [500]
  let y = x + x + x :: SWord32
  cgOutput "y" y

