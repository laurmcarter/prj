{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.K where

import Control.Applicative
import Control.Monad.Cont
import Control.Lens

type K e r = Cont (Either e r)

throw :: e -> K e r a
throw = cont . const . Left

catch :: K e r a -> (e -> K e r a) -> K e r a
catch m h = cont $ \k ->
  case runCont m k of
    Right a -> return a
    Left  e -> runCont (h e) k

abort :: r -> K e r a
abort = cont . const . Right

