module Control.Monad.Cont.Abort where

import Control.Applicative
import Control.Monad
import Data.Monoid

-- | @ContAbort fl r a@ is a continuation monad with two continuations available:
-- an abort continuation of type @(fl -> r)@, and a successful continuation of type @(a -> r)@.
newtype ContAbort fl r a = ContAbort
  { runContAbort :: (fl -> r) -> (a -> r) -> r
  }

instance Functor (ContAbort fl r) where
  fmap f m = ContAbort $ \out cont -> runContAbort m out (cont . f)

instance Monad (ContAbort fl r) where
  return a = ContAbort $ \_ cont -> cont a
  m >>= f  = ContAbort $ \out cont ->
    runContAbort m out $ \a -> runContAbort (f a) out cont

-- | In general, @ContAbort fl r@ is not a MonadPlus, unless @fl@ has a default value available.
-- Such is the case when @fl@ is a 'Monoid', and 'mappend' can be leveraged to join two failure
-- results together.
instance Monoid fl => MonadPlus (ContAbort fl r) where
  mzero = abort mempty
  mplus m1 m2 = ContAbort $ \out cont ->
    flip (runContAbort m1) cont $ \err ->
      flip (runContAbort m2) cont $ \err' -> out (err <> err')

instance Applicative (ContAbort fl r) where
  pure  = return
  (<*>) = ap

instance Monoid fl => Alternative (ContAbort fl r) where
  empty = mzero
  (<|>) = mplus

abort :: fl -> ContAbort fl r a
abort fl = ContAbort $ \out _ -> out fl

-- | @callCCA@ (call-with-current-ContAbort) allows for either the success or failure continuation to be called, through the (Either fl a) type.
callCCA :: ((Either fl a -> ContAbort fl r b) -> ContAbort fl r a) -> ContAbort fl r a
callCCA f = ContAbort $ \out cont ->
  runContAbort (f $ \a -> ContAbort $ \_ _ -> either out cont a) out cont

-- | @callCCAForget@ forgets the abort continuation.
callCCAForget :: ((a -> ContAbort fl' r b) -> ContAbort fl r a) -> ContAbort fl r a
callCCAForget f = ContAbort $ \out cont ->
  runContAbort (f $ \a -> ContAbort $ \_ _ -> cont a) out cont

type Handler a r = (a -> r) -> a -> r

-- | Intercept the handling of both successes and failures.
handle :: ContAbort fl r a -> Handler fl r -> Handler a r -> ContAbort fl r a
handle m outH contH = ContAbort $ \out cont ->
  runContAbort m
    (outH out)
    (contH cont)

-- | Intercept failure results.
catchAbort :: ContAbort fl r a -> Handler fl r -> ContAbort fl r a
m `catchAbort` h = handle m h id

-- | Intercept success results.
catchCont :: ContAbort fl r a -> Handler a r -> ContAbort fl r a
m `catchCont` h = handle m id h

