{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Symbol where

import Data.Type.Equality
import qualified GHC.TypeLits as Lit

{-
-- {{{

data C
  = Ca Bool | Cb Bool | Cc Bool | Cd Bool
  | Ce Bool | Cf Bool | Cg Bool | Ch Bool
  | Ci Bool | Cj Bool | Ck Bool | Cl Bool
  | Cm Bool | Cn Bool | Co Bool | Cp Bool
  | Cq Bool | Cr Bool | Cs Bool | Ct Bool
  | Cu Bool | Cv Bool | Cw Bool | Cx Bool
  | Cy Bool | Cz Bool
  | D0 | D1 | D2 | D3 | D4
  | D5 | D6 | D7 | D8 | D9
  | CPer | CSpc | CHyp | CUnd
  | CBng | CAt  | COct | CDol
  | 

-- Char Equality {{{

type instance a == b = CharEq a b

type family CharEq (a :: C) (b :: C) :: Bool where
  CharEq (Ca a) (Ca b) = a == b
  CharEq (Cb a) (Cb b) = a == b
  CharEq (Cc a) (Cc b) = a == b
  CharEq (Cd a) (Cd b) = a == b
  CharEq (Ce a) (Ce b) = a == b
  CharEq (Cf a) (Cf b) = a == b
  CharEq (Cg a) (Cg b) = a == b
  CharEq (Ch a) (Ch b) = a == b
  CharEq (Ci a) (Ci b) = a == b
  CharEq (Cj a) (Cj b) = a == b
  CharEq (Ck a) (Ck b) = a == b
  CharEq (Cl a) (Cl b) = a == b
  CharEq (Cm a) (Cm b) = a == b
  CharEq (Cn a) (Cn b) = a == b
  CharEq (Co a) (Co b) = a == b
  CharEq (Cp a) (Cp b) = a == b
  CharEq (Cq a) (Cq b) = a == b
  CharEq (Cr a) (Cr b) = a == b
  CharEq (Cs a) (Cs b) = a == b
  CharEq (Ct a) (Ct b) = a == b
  CharEq (Cu a) (Cu b) = a == b
  CharEq (Cv a) (Cv b) = a == b
  CharEq (Cw a) (Cw b) = a == b
  CharEq (Cx a) (Cx b) = a == b
  CharEq (Cy a) (Cy b) = a == b
  CharEq (Cz a) (Cz b) = a == b
  CharEq  D0     D0    = True
  CharEq  D1     D1    = True
  CharEq  D2     D2    = True
  CharEq  D3     D3    = True
  CharEq  D4     D4    = True
  CharEq  D5     D5    = True
  CharEq  D6     D6    = True
  CharEq  D7     D7    = True
  CharEq  D8     D8    = True
  CharEq  D9     D9    = True
  CharEq  a      b     = False

-- }}}

data Chr (c :: C) where
  Ca Bool 
  Cb Bool 
  Cc Bool 
  Cd Bool
  Ce Bool 
  Cf Bool 
  Cg Bool 
  Ch Bool
  Ci Bool 
  Cj Bool 
  Ck Bool 
  Cl Bool
  Cm Bool 
  Cn Bool 
  Co Bool 
  Cp Bool
  Cq Bool 
  Cr Bool 
  Cs Bool 
  Ct Bool
  Cu Bool 
  Cv Bool 
  Cw Bool 
  Cx Bool
  Cy Bool 
  Cz Bool
  D0 
  D1 
  D2 
  D3 
  D4
  D5 
  D6 
  D7 
  D8 
  D9

-- }}}
-}

