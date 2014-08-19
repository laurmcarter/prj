{-# LANGUAGE ScopedTypeVariables #-}

module Text.Show.Supply where

import Control.Arrow
import Data.Supply
import Data.Proxy
import Data.List (intersperse)

type ShowG g = (g,String) -> (g,String)

class ShowSupply a where
  showgPrec   :: forall g. Supply g => Int -> a -> ShowG g
  showgPrec _  = showgString . showg (Proxy :: Proxy g)
  showsg      :: Supply g => a -> ShowG g
  showsg       = showgPrec 0
  showg       :: Supply g => prx g -> a -> String
  showg    (_ :: p g) a = snd $ showgPrec 0 a ((newSupply :: g),"")
  showgList   :: Supply g => [a] -> ShowG g
  showgList as = wrapgChar '[' ']' $ sepgChar ',' $ map showsg as
  {-# MINIMAL showgPrec | showg #-}

-- Operations {{{

showS_to_G :: ShowS -> ShowG g
showS_to_G = second

showG_to_S :: Supply g => ShowG g -> ShowS
showG_to_S f s = s'
  where
  (_,s') = f (newSupply,s)

showgString :: String -> ShowG g
showgString = showS_to_G . showString

showgChar :: Char -> ShowG g
showgChar = showS_to_G . showChar

showgParen :: Bool -> ShowG g -> ShowG g
showgParen b p = if b
  then wrapgChar '(' ')' p
  else p

fresh :: Supply g => (Fresh g -> ShowG g) -> ShowG g
fresh f (g,s) = f x (g',s)
  where
  (x,g') = split g

wrapgChar :: Char -> Char -> ShowG g -> ShowG g
wrapgChar o c p = showgChar o . p . showgChar c

wrapgString :: String -> String -> ShowG g -> ShowG g
wrapgString o c p = showgString o . p . showgString c

sepgChar :: Char -> [ShowG g] -> ShowG g
sepgChar c = compose . intersperse (showgChar c)

sepgString :: String -> [ShowG g] -> ShowG g
sepgString s = compose . intersperse (showgString s)

unwordsg :: [ShowG g] -> ShowG g
unwordsg = sepgChar ' '

unlinesg :: [ShowG g] -> ShowG g
unlinesg = sepgChar '\n'

indentg :: Int -> [ShowG g] -> [ShowG g]
indentg n = map (showgString (replicate n ' ') .)

unindentg :: Int -> [ShowG g] -> [ShowG g]
unindentg = map . go
  where
  go :: Int -> ShowG g -> ShowG g
  go n f p@(g,s) = (g',loop n res)
    where
    (g',res) = f p
    loop :: Int -> String -> String
    loop 0 s = s
    loop n s = case s of
      ' ':s' -> loop (pred n) s'
      _      -> s

-- }}}

-- ShowS Conversion {{{

newtype LiftedShow a = LiftShow
  { unliftShow :: a
  }

newtype LoweredShow g a = LowerShow
  { unlowerShow :: a
  }

instance Show a => ShowSupply (LiftedShow a) where
  showgPrec d (LiftShow a) = showgParen (d > 10)
    $ showgString "LiftShow "
    . showgString (show a)

instance (ShowSupply a, Supply g) => Show (LoweredShow g a) where
  showsPrec d (LowerShow a) = showParen (d > 10)
    $ showString "LowerShow "
    . showG_to_S (showsg a :: ShowG g)

-- }}}

