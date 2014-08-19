
module Data.Staged.Length where

import Data.Staged.Class
import Prelude hiding (length)
import qualified Prelude

newtype Length a = Length
  { length :: Int
  } deriving (Eq,Ord,Show)

-- Instances {{{

instance Phantom Length where
  retype = Length . length

instance Num (Length a) where
  fromInteger = Length . fromInteger
  x + y       = Length $ succ $ length x +: length y
  x * y       = Length $ succ $ length x +: length y
  x - y       = Length $ succ $ length x +: length y
  abs    x    = Length $ succ $ length x
  signum x    = Length $ succ $ length x

instance Enum (Length a) where
  toEnum   = Length
  fromEnum = length

-- }}}

instance Sym Length where
  bool _    = 1
  ----
  lam f     = retype $ succ $ f 0
  app f x   = succ $ retype f +: retype x
  fix f     = retype $ succ $ f 0
  ----
  -- x + y     = succ $ retype x +: retype y
  -- x * y     = succ $ retype x +: retype y
  -- x - y     = succ $ retype x +: retype y
  x <= y    = succ $ retype x +: retype y
  ----
  if_ t c a = succ $ retype t +: retype c +: retype a

