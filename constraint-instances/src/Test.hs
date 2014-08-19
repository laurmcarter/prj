{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test where

import Data.Constraint.Instance

deriveRecovery "Show (a,b)"

