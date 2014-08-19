{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test where

import Control.Lens
import Control.Lens.Poly hiding (Dbl(..))
import Control.Lens.Poly.TH

data Dbl = Dbl

makePolyIso [d|
  isoAt :: Iso s t (s,s) (t,t)
  isoAt Dbl = iso (\s->(s,s)) (\(t,_)->t)
  |]

