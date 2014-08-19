{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

import PTypes
import PMap
import Printf

import Control.Applicative ((<$>),(<*>))

explore :: Ord k => Maybe Int -> PV k -> PV k
explore maxDepth choices = foldi (\v p a -> PV $ (p,V v):runPV a) susp ans
  where
  loop :: Ord k => Prob -> Int -> Bool -> PV k -> (PMap k Prob, PV k) -> (PMap k Prob, PV k)
  loop p depth down choices answers@(ans,susp) = case runPV choices of
    []            -> answers
    (pt,V v):rest -> loop p depth down (PV rest) (insertWith (+) v (pt * p) ans,susp)
    (pt,C t):rest -> if down
      then loop p depth down (PV rest) $
        loop (pt * p) (depth+1) down' t answers
      else loop p depth down (PV rest) (ans,PV $ (pt * p,C t):runPV susp)
      where
      down' = maybe True (depth <) maxDepth
  (ans,susp) = loop 1.0 0 True choices (empty,PV [])

nearlyOne = 1.0 - 1e-7

shallowExplore :: Ord k => Int -> PV k -> PV k
shallowExplore maxDepth choices = foldi (\v p a -> PV $ (p, V v): runPV a) susp ans
  where
  addAnswer :: Ord k => Prob -> k -> PMap k Prob -> PMap k Prob
  addAnswer pContrib v m = insertWith (+) v pContrib m
  loop :: Ord k => Prob -> Int -> PMap k Prob -> PV k -> PV k -> (PMap k Prob,PV k)
  loop pc depth ans acc ch = case runPV ch of
    [] -> (ans,acc)
    (p,V v):rest -> loop pc depth (addAnswer (p * pc) v ans) acc (PV rest)
    (p,C t):rest -> loop pc depth ans' acc' (PV rest)
      where
      (ans',ch') = loop (pc * p) (depth+1) ans (PV []) t
      pTotal :: Prob
      pTotal    = foldl (\pa (p,_) -> pa + p) 0.0 $ runPV ch'
      acc'      = if pTotal == 0 then acc
        else if pTotal < nearlyOne
          then PV $ (p * pTotal, C (PV $ map (\(p,x) -> (p / pTotal, x)) $ runPV ch')) : runPV acc
          else PV $ (p,C ch') : runPV acc
  (ans,susp) = loop 1.0 0 empty (PV []) choices

firstSuccess :: Ord k => PV k -> PV k
firstSuccess choices = case runPV choices of
  []            -> PV []
  (_,V _):_     -> choices
  (pt,C t):rest -> firstSuccess $ PV (rest ++ map (\(p,v) -> (pt * p,v)) (runPV t))

boundedExplore :: Ord k => Int -> PV k -> (Prob,Prob)
boundedExplore maxSize = loop True 1 0 0 empty 0
  where
  loop :: Ord k => Bool -> Prob -> Prob -> Prob -> PMap Prob [PV k] -> Int -> PV k -> (Prob,Prob)
  loop explore pc low high jqueue jqsize choices = case runPV choices of
    [] -> next low high jqueue jqsize
    (p,V _):rest -> let pe = pc * p in
      loop explore pc (low + pe) (high + pe) jqueue jqsize (PV rest)
    (p,C t):rest -> if explore
      then loop explore pc low high (insertWith (++) (pc * p) [t] jqueue) (jqsize+1) (PV rest)
      else loop explore pc low (high + pc * p) jqueue jqsize (PV rest)

  next :: Ord k => Prob -> Prob -> PMap Prob [PV k] -> Int -> (Prob,Prob)
  next low high jqueue jqsize = if jqsize == 0
    then (low,high)
    else if jqsize < maxSize
    then let ((p,t:ts),jqueue') = deleteFindMax jqueue
             jqueue'' = if ts == [] then jqueue' else add p ts jqueue
          in
      loop True p low high jqueue'' (jqsize-1) t
    else let ((p,t:ts),jqueue') = deleteFindMax jqueue
             jqueue'' = if ts == [] then jqueue' else add p ts jqueue
          in
      loop False p low high jqueue'' (jqsize-1) t

rejectionSampleDist :: Ord k => Selector (VC k) -> Int -> PV k -> PV k
rejectionSampleDist sel nSamples ch = driver ch empty nSamples
  where
  loop :: Ord k => Selector (VC k) -> Prob -> PMap k Prob -> PV k -> PMap k Prob
  loop sel pContrib ans ch = case runPV ch of
    [] -> ans
    [(p,V v)] -> insertWith (+) v (p * pContrib) ans
    [(p,C t)] -> loop sel (p * pContrib) ans t
    _ -> let (pTotal,t) = runSelector sel (pvToCDist ch) in
      loop sel (pContrib * pTotal) ans (PV [(1,t)])
  driver ch ans n = undefined

pvToCDist :: PV k -> CDist (VC k)
pvToCDist = CDist . runPV



