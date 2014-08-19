
module Main where

import Options.Applicative

data Test = Test
  { foo  :: Double
  , bar  :: Bool
  , baz  :: Maybe Int
  , quux :: Bool
  } deriving (Eq,Show)

config :: Parser Test
config = Test
  <$> option
   .$ long "foo"
   <> short 'f'
   <> metavar "Power Level"
  <*> switch
   .$ long "bar"
   <> short 'b'
  <*> optional
   .$ option
   .$ long "baz"
   <> short 'z'
   <> metavar "Bustedness"
  <*> switch
   .$ long "quux"
   <> short 'q'

(.$) :: (a -> b) -> a -> b
f .$ x = f x
infixr 5 .$

main :: IO ()
main = do
  cfg <- execParser opts
  print cfg
  where
  opts = info config
     $ fullDesc
    <> progDesc "Do the thing, Ju-Li."
    <> header "Do it."

{-

pure 1 <^(*)^> pure 2

(pure 1 <^ (*)) ^> pure 2
_______                   :: f Int
           ___            :: Int -> Int -> Int
______________            :: f (Int -> Int)
                   ______ :: f Int
_________________________ :: f Int

-}

