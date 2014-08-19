module NDSM where

import Control.Monad

newtype NDSM s a = NDSM {runNDSM :: s -> [(s,a)]}

instance Monad (NDSM s) where
	return x = NDSM (\s -> [(s,x)])
	(NDSM f) >>= c = NDSM (\s -> let svl = f s
					 ml = map (\(_,x)->(runNDSM $ c x)) svl
					 sl = map fst svl
					 aml = zipWith ($) ml sl
				     in concat aml)


instance MonadPlus (NDSM s) where
	mzero = NDSM (const [])
	mplus (NDSM x) (NDSM y) = NDSM (\s->(x s)++(y s))


onState :: (s -> s) -> NDSM s ()
onState f = NDSM (\s->[(f s,())])

fromState :: (s -> a) -> NDSM s a
fromState f = NDSM (\s->[(s, f s)])

