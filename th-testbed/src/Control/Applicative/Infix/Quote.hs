{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Applicative.Infix.Quote where

import Control.Applicative
import Control.Lens

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta

a :: QuasiQuoter
a = QuasiQuoter
  { quoteExp  = appExp
  , quotePat  = stub
  , quoteType = stub
  , quoteDec  = stub
  }
  where
  stub = error "unprovided"

appExp :: String -> Q Exp
appExp s = [| liftA2 $ex |]
  where
  ex = handleEither $ parseExp s

handleEither :: Monad m => Either String a -> m a
handleEither = either fail return

