{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}

module Data.Map.Static where

import Data.Static

import Control.Applicative
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Instances.TH.Lift
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Data (Data)

staticMap :: (Data k, Data a, Ord k, Lift k, Lift a) => String
  -> Map k a
  -> Q [Dec]
  -- ^ lookup function :: String -> Maybe k
  -> Q [Dec]
staticMap mn mp luQ = do
  [inMap,stub] <- mapM newName ["inMap","stub"]
  let mapNm = mkName mn
  sequence
    [ sigD mapNm $ conT ''QuasiQuoter
    , funD mapNm
      [ do luQ' <- luQ
           lkup <- getFnName luQ'
           clause []
             ( normalB
             $ recConE 'QuasiQuoter
               [ (,) 'quoteExp  <$> appE (varE inMap) (varE 'dataToExpQ)
               , (,) 'quotePat  <$> appE (varE inMap) (varE 'dataToPatQ)
               , (,) 'quoteType <$> varE stub
               , (,) 'quoteDec  <$> varE stub
               ]
             )
             $ map return luQ' ++
               [ funD stub [ clause [] (normalB [|error "unprovided"|]) [] ]
               , sigD inMap [t| forall r. ( forall a. Data a => (forall b. Data b => b -> Maybe (Q r)) -> a -> Q r) -> String -> Q r |]
               , do [f,s,m] <- mapM newName ["f","s","m"]
                    funD inMap
                      [ clause [varP f, varP s]
                        ( normalB
                        $ caseE (appE (varE lkup) (varE s))
                          [ do k <- newName "k"
                               match (conP 'Just [varP k])
                                 ( normalB
                                 $ appE (appE (varE f) [|const Nothing|]) (uInfixE (varE m) (varE '(M.!)) (varE k))
                                 )
                                 []
                          , match wildP
                            ( normalB
                            $ appE (varE 'fail) (uInfixE [|"Couldn't parse key: "|] (varE '(++)) (varE s))
                            )
                            []
                          ]
                        )
                        -- where
                        [ sigD m mt
                        , funD m [ clause [] (normalB me) [] ]
                        ]
                      ]
               ]
      ]
    ]
    where
    mt :: Q Type
    mt = typeOfQ mp
    me :: Q Exp
    me = lift mp
    kt :: Q Type
    kt = mt >>= maybe (fail "Couldn't find Map key type") return . go
      where
      go :: Type -> Maybe Type
      go typ = case typ of
        ForallT _ _ t -> go t
        AppT (ConT m) t
          | m == ''Data.Map.Map
          -> return t
          | otherwise
          -> go t
        AppT t1 t2 -> go t1 `mplus` go t2
        SigT t _ -> go t
        _ -> Nothing

    getFnName :: [Dec] -> Q Name
    getFnName ds = do
      case find go ds of
        Just (SigD nm _) -> return nm
        Just (FunD nm _) -> return nm
        _                -> fail "Couldn't find lookup function name"
      where
      go :: Dec -> Bool
      go d = case d of
        SigD {} -> True
        FunD {} -> True
        _       -> False

