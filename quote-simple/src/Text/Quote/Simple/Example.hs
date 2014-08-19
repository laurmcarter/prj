{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Quote.Simple.Example where

import Text.Quote.Simple
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Language.Haskell.Exts as HS

import Control.Arrow (left)
import Control.Monad
import Control.IxMonad
import Data.Text (Text)
import qualified Data.Text as T

ido :: QuasiQuoter
ido  = QuasiQuoter
  { quoteExp  = wrapIxMonad . quoteEmbedded mkDoBlock wrapPreludeMonad
  , quotePat  = stub
  , quoteType = stub
  , quoteDec  = stub
  }
  where
  stub = error "stubbed."

mkDoBlock :: Text -> Text
mkDoBlock = T.append "do\n"

wrapPreludeMonad :: ExpQ -> ExpQ
wrapPreludeMonad = letE
  $ mkBinds
    [ ( "return" , [| Prelude.return :: Monad m => a -> m a                 |] )
    , ( ">>="    , [| (Prelude.>>=)  :: Monad m => m a -> (a -> m b) -> m b |] )
    , ( ">>"     , [| (Prelude.>>)   :: Monad m => m a       -> m b  -> m b |] )
    , ( "fail"   , [| Prelude.fail   :: Monad m => String -> m a            |] )
    ]

wrapIxMonad :: ExpQ -> ExpQ
wrapIxMonad = letE
  $ mkBinds
    [ ( "return" , [| Control.IxMonad.ireturn :: (IxPointed m, IxBind m) => a -> m i i a                         |] )
    , ( ">>="    , [| (Control.IxMonad.>>>=)  :: (IxPointed m, IxBind m) => m i j a -> (a -> m j k b) -> m i k b |] )
    , ( ">>"     , [| (Control.IxMonad.>>>)   :: (IxPointed m, IxBind m) => m i j a -> m j k b -> m i k b        |] )
    , ( "fail"   , [| Prelude.error           :: (IxPointed m, IxBind m) => String -> m i i a                    |] )
    ]

mkBinds :: [(String,ExpQ)] -> [DecQ]
mkBinds = map $ uncurry mkBind

mkBind :: String -> ExpQ -> DecQ
mkBind x = funD (mkName x) . mkClause

mkClause :: ExpQ -> [ClauseQ]
mkClause e' = [clause [] (normalB e') []]

