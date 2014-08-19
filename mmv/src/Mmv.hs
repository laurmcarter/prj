{-# LANGUAGE NoImplicitPrelude #-}

import ClassyPrelude
import Run

main :: IO ()
main = runWithArgs mmvFlags () $ \_ args ->

mmvFlags :: Flags Mmv
mmvFlags = Flags mempty $ \rest -> case rest of
  [from,to] -> returnM $ Mmv from to
  _ -> failM $ usage

data Mmv = Mmv
  { from :: Text
  , to   :: Text
  } deriving (Eq,Ord,Show)

