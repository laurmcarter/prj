
module PTypes where

type Prob = Double

newtype Dist a  = Dist
  { runDist :: [(Prob,a)]
  } deriving (Eq,Show)
newtype CDist a = CDist
  { runCDist :: [(Prob,a)]
  } deriving (Eq,Show)

data VC a
  = V a
  | C (PV a)
  deriving (Eq,Show)

instance Functor VC where
  fmap f vc = case vc of
    V a -> V $ f a
    C pv -> C $ fmap f pv

newtype PV a = PV
  { runPV :: [(Prob, VC a)]
  } deriving (Eq,Show)

instance Functor PV where
  fmap f (PV ls) = PV $ map (\(p,vc) -> (p,fmap f vc)) ls

newtype Selector a = Selector
  { runSelector :: CDist a -> (Prob,a)
  }

