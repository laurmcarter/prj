{-# LANGUAGE DataKinds #-}

module Data.Nat.Proxies where

import Data.Nat
import Data.Proxy

type Zero     = Z
type One      = S Zero
type Two      = S One
type Three    = S Two
type Four     = S Three 
type Five     = S Four
type Six      = S Five
type Seven    = S Six
type Eight    = S Seven
type Nine     = S Eight

zero  = Proxy :: Proxy Zero
one   = Proxy :: Proxy One
two   = Proxy :: Proxy Two
three = Proxy :: Proxy Three
four  = Proxy :: Proxy Four
five  = Proxy :: Proxy Five
six   = Proxy :: Proxy Six
seven = Proxy :: Proxy Seven
eight = Proxy :: Proxy Eight
nine  = Proxy :: Proxy Nine

