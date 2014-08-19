{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}

module Data.Static
  ( module Data.Static
  , lift
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Data.Data (Data,Typeable,TypeRep)
import qualified Data.Data as Data

-- generalized signature of the dataTo*Q functions
type MkQ r =  Data a => (forall b. Data b => b -> Maybe (Q r)) -> a -> Q r

typeOfQ :: Typeable a => a -> Q Type
typeOfQ = typeRepQ . Data.typeOf

typeRepQ :: TypeRep -> Q Type
typeRepQ (Data.splitTyConApp -> ((Data.tyConName -> tc),ts)) = do
  mt  <- lookupTypeName tc
  case mt of
    Just t -> foldl appT (conT t) $ map typeRepQ ts
    _      -> fail $ "Unknown type: " ++ tc

st :: QuasiQuoter
st  = QuasiQuoter
  { quoteExp  = staticExp
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

staticExp :: String -> Q Exp
staticExp s = case parseExp s of
  Left err -> fail err
  Right e  -> [| lift $(return e) |]

