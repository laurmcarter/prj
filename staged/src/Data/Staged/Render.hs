{-# LANGUAGE FlexibleInstances #-}

module Data.Staged.Render where

import Data.Staged.Class
import Prelude hiding (showParen,showString,showChar)
import qualified Prelude

newtype Render g a = Render
  { rendersPrec :: Int -> ShowG g
  }

-- Instances and Ops {{{

instance Phantom (Render g) where
  retype = Render . rendersPrec

pRender :: Render Vars a -> IO ()
pRender = putStrLn . render

render :: Render Vars a -> String
render r = snd $ renders r (initialSupply,"")

renders :: Render g a -> ShowG g
renders r = rendersPrec r 0

var :: String -> Render g a
var = Render . const . showString

instance Num (Render g Int)
  -- x + y     = Render $ \d ->
  --     showParen (d > 6)
  --   $ rendersPrec x 7
  --   . showString " + "
  --   . rendersPrec y 7
  -- x * y     = Render $ \d ->
  --     showParen (d > 7)
  --   $ rendersPrec x 8
  --   . showString " * "
  --   . rendersPrec y 8
  -- x - y     = Render $ \d ->
  --     showParen (d > 6)
  --   $ rendersPrec x 7
  --   . showString " - "
  --   . rendersPrec y 7

-- }}}

instance Supply g => Sym (Render g) where
  bool      = Render . const . showg
  -- int       = Render . const . showg
  ----
  lam f     = Render $ \d ->
      fresh $ \x ->
      showParen (d > 10)
    $ showString ("λ" ++ x ++ " -> ")
    . renders (f $ var x)
  app f x   = Render $ \d ->
      showParen (d > 10)
    $ rendersPrec f 11
    . showChar ' '
    . rendersPrec x 11
  fix f     = Render $ \d ->
      fresh $ \x ->
      showParen (d > 10)
    $ showString ("μ" ++ x ++ " -> ")
    . renders (f $ var x)
  ----
  -- x + y     = Render $ \d ->
  --     showParen (d > 6)
  --   $ rendersPrec x 7
  --   . showString " + "
  --   . rendersPrec y 7
  -- x * y     = Render $ \d ->
  --     showParen (d > 7)
  --   $ rendersPrec x 8
  --   . showString " * "
  --   . rendersPrec y 8
  -- x - y     = Render $ \d ->
  --     showParen (d > 6)
  --   $ rendersPrec x 7
  --   . showString " - "
  --   . rendersPrec y 7
  x <= y    = Render $ \d ->
      showParen (d > 4)
    $ rendersPrec x 5
    . showString " <= "
    . rendersPrec y 5
  ----
  if_ t c a = Render $ \d ->
      showParen (d > 0)
    $ showString "if "
    . renders t
    . showString " then "
    . renders c
    . showString " else "
    . renders a

-- ShowG {{{

type ShowG g = (g,String) -> (g,String)

showG :: ShowS -> ShowG g
showG f (g,s) = (g,f s)

class Supply g where
  initialSupply :: g
  fresh :: (String -> ShowG g) -> ShowG g

instance Supply Int where
  initialSupply = 0
  fresh f (g,s) = f ("x" ++ show g) $ (succ g,s)

newtype Vars = Vars { runVars :: [String] } deriving (Eq,Ord,Show)

instance Supply Vars where
  initialSupply = Vars $ vs ++ [ v ++ show i | i <- [0..] , v <- vs ]
    where
    vs = map (:[]) ['a'..'z']
  fresh f (g,s) = case runVars g of
    []     -> error "Empty supply"
    x : g' -> f x $ (Vars g',s)

showg :: Show a => a -> ShowG g
showg = showG . shows

showString :: String -> ShowG g
showString = showG . Prelude.showString

showChar :: Char -> ShowG g
showChar = showG . Prelude.showChar

showParen :: Bool -> ShowG g -> ShowG g
showParen d p = if d
  then showChar '(' . p . showChar ')'
  else p

-- }}}

