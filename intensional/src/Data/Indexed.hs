{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Data.Indexed where

import Control.Applicative hiding ((<**>))
import Control.Monad.Fix
import GHC.Exts (Constraint)
import Type.Type


-- Indexed

-- Functor Empty {{{

class IxFunctor f where
  imap :: (a -> b) -> f i j a -> f i j b

(<$$>) :: IxFunctor f => (a -> b) -> f i j a -> f i j b
(<$$>) = imap
infixl 4 <$$>

(<$$) :: IxFunctor f => a -> f i j b -> f i j a
a <$$ f = const a <$$> f
infixl 4 <$$

($$>) :: IxFunctor f => f i j b -> a -> f i j a
($$>) = flip (<$$)

ivoid :: IxFunctor f => f i j a -> f i j ()
ivoid = (() <$$)

class IxFunctor f => IxEmpty f where
  iempty :: f i i a

-- }}}

-- Pointed Apply Applicative Alt Alternative {{{

class IxFunctor m => IxPointed m where
  ireturn :: a -> m i i a

class IxFunctor m => IxApply m where
  iap    :: m i j (a -> b) -> m j k a -> m i k b
  ithenL :: m i j a        -> m j k b -> m i k a
  ithenL = iliftA2 const
  ithenR :: m i j a        -> m j k b -> m i k b
  ithenR = iliftA2 (const id)

(<**>) :: IxApply m => m i j (a -> b) -> m j k a -> m i k b
(<**>) = iap
infixl 4 <**>

(<**) :: IxApply m => m i j a -> m j k b -> m i k a
(<**) = ithenL
infixl 4 <**

(**>) :: IxApply m => m i j a -> m j k b -> m i k b
(**>) = ithenR
infixl 4 **>

type IxApplicative m = (IxPointed m, IxApply m)

iliftA :: IxApply m => (a -> b) -> m i j a -> m i j b
iliftA = (<$$>)

iliftA2 :: IxApply m => (a -> b -> c) -> m i j a -> m j k b -> m i k c
iliftA2 f a b = f <$$> a <**> b

iliftA3 :: IxApply m => (a -> b -> c -> d)
  -> m i j a -> m j k b -> m k l c -> m i l d
iliftA3 f a b c = f <$$> a <**> b <**> c

class IxApplicative m => IxAlt m where
  ialt  :: m i j a -> m i j a -> m i j a
  isome :: m i j a -> m i j [a]
  imany :: m i j a -> m i j [a]

(<||>) :: IxAlt m => m i j a -> m i j a -> m i j a
(<||>) = ialt

type IxAlternative m = (IxEmpty m, IxAlt m)

-- }}}

-- Bind Monad MonadZero MonadAdd MonadPlus MonadFix {{{

class IxApply m => IxBind m where
  ibind :: m i j a -> (a -> m j k b) -> m i k b

ijoin :: IxBind m => m i j (m j k a) -> m i k a
ijoin = flip ibind id

(>>>=) :: IxBind m => m i j a -> (a -> m j k b) -> m i k b
(>>>=) = ibind
infixr 1 >>>=

(=<<<) :: IxBind m => (a -> m j k b) -> m i j a -> m i k b
k =<<< m = ibind m k
infixl 1 =<<<

(>>>) :: IxBind m => m i j a -> m j k b -> m i k b
m1 >>> m2 = ibind m1 $ \_ -> m2
infixl 1 >>>

(>>=>) :: IxBind m => (a -> m i j b) -> (b -> m j k c) -> a -> m i k c
(f >>=> g) x = f x >>>= g
infixr 1 >>=>

(<=<<) :: IxBind m => (b -> m j k c) -> (a -> m i j b) -> a -> m i k c
(f <=<< g) x = g x >>>= f
infixr 1 <=<<

type IxMonad m = (IxPointed m, IxBind m)

iapIxMonad :: IxMonad m => m i j (a -> b) -> m j k a -> m i k b
iapIxMonad f x = f >>>= \f' ->
  x >>>= \x' ->
    ireturn (f' x')

iliftM :: IxMonad m => (a -> b) -> m i j a -> m i j b
iliftM = iliftA

iliftM2 :: IxMonad m => (a -> b -> c) -> m i j a -> m j k b -> m i k c
iliftM2 = iliftA2

iliftM3 :: IxMonad m => (a -> b -> c -> d) -> m i j a -> m j k b -> m k l c -> m i l d
iliftM3 = iliftA3

isequence :: IxMonad m => [m i i a] -> m i i [a]
isequence = foldr (iliftM2 (:)) $ ireturn []

imapM :: IxMonad m => (a -> m i i b) -> [a] -> m i i [b]
imapM f = isequence . map f

isequence_ :: IxMonad m => [m i i a] -> m i i ()
isequence_ = foldr (>>>) $ ireturn ()

imapM_ :: IxMonad m => (a -> m i i b) -> [a] -> m i i ()
imapM_ f = isequence_ . map f

class IxMonad m => IxMonadZero m where
  imzero :: m i j a

class IxMonad m => IxMonadAdd m where
  implus :: m i j a -> m i j a -> m i j a

type IxMonadPlus m = (IxMonadZero m, IxMonadAdd m)

class IxMonad m => IxMonadFix m where
  imfix :: (a -> m i i a) -> m i i a

-- }}}

-- Copointed Coapply Coapplicative Coalt Coalternative {{{

class IxFunctor w => IxCopointed w where
  iextract :: w i i a -> a

class IxFunctor w => IxCoapply w where
  icoap    :: w j k (a -> b) -> w i k a -> w i j b
  icothenL :: w j k a -> w i k b -> w i j a
  icothenR :: w j k a -> w i k b -> w i j b

(<@@>) :: IxCoapply w => w j k (a -> b) -> w i k a -> w i j b
(<@@>) = icoap
infixl 4 <@@>

(<@@) :: IxCoapply w => w j k a -> w i k b -> w i j a
(<@@) = icothenL
infixl 4 <@@

(@@>) :: IxCoapply w => w j k a -> w i k b -> w i j b
(@@>) = icothenR
infixl 4 @@>

type IxCoapplicative w = (IxCopointed w, IxCoapply w)

class IxCoapplicative w => IxCoalt w where
  icoalt  :: w i j a -> (w i j a,w i j a)
  icosome :: w i j a -> [w i j a]
  icomany :: w i j a -> [w i j a]

type IxCoalternative w = (IxEmpty w, IxCoapplicative w)

-- }}}

-- Extend Comonad ComonadZero ComonadSub ComonadMinus ComonadCofix {{{

class IxFunctor w => IxExtend w where
  iextend :: (w j k a -> b) -> w i k a -> w i j b

iduplicate :: IxExtend w => w i k a -> w i j (w j k a)
iduplicate = iextend id

type IxComonad w = (IxCopointed w, IxExtend w)

(=>>>) :: IxExtend w => (w j k a -> b) -> w i k a -> w i j b
(=>>>) = iextend
infixl 1 =>>>

(<<<=) :: IxExtend w => w i k a -> (w j k a -> b) -> w i j b
(<<<=) = flip (=>>>)
infixr 1 <<<=

(=>>=) :: IxExtend w => (w j k a -> b) -> (w i j b -> c) -> w i k a -> c
f =>>= g = g . iextend f
infixl 1 =>>=

(=<<=) :: IxExtend w => (w i j b -> c) -> (w j k a -> b) -> w i k a -> c
f =<<= g = f . iextend g
infixr 1 =<<=

class IxComonad w => IxComonadZero w where
  iwzero :: w i j a

class IxComonadSub w where
  iwminus :: w i j a -> (w i j a,w i j a)

type IxComonadMinus w = (IxComonadZero w, IxComonadSub w)

class IxComonad w => IxComonadCofix w where
  iwfix :: (w i i a -> a) -> w i i a

-- }}}

-- Unindexed

-- Empty {{{
    
class Functor f => Empty f where
  empty :: f a

-- }}}

-- Copointed Coapply Coapplicative Coalt Coalternative {{{

class Functor w => Copointed w where
  extract :: w a -> a

class Functor w => Coapply w where
  coap :: w (a -> b) -> w a -> w b

type Coapplicative w = (Copointed w, Coapply w)

class Coapplicative w => Coalt w where
  coalt  :: w a -> (w a,w a)
  cosome :: w a -> [w a]
  comany :: w a -> [w a]

type Coalternative w = (Empty w, Coalt w)

-- }}}

-- MonadZero MonadAdd {{{

class Monad m => MonadZero m where
  zero :: m a

class Monad m => MonadAdd m where
  madd :: m a -> m a -> m a

-- }}}

-- Extend Comonad ComonadZero ComonadSub ComonadMinus {{{

class Functor w => Extend w where
  extend :: (w a -> b) -> w a -> w b

type Comonad w = (Copointed w, Extend w)

-- }}}

{-
-- Wrapper {{{

newtype Ixed i j f a = WrapIx 
  { unwrapIx :: f a
  } deriving (Eq,Show)

instance IxFunctor f i j => Functor (Ixed i j f) where
  fmap f = WrapIx . imap f . unwrapIx

instance IxApplicative m => Applicative (Ixed i i m) where
  pure = WrapIx . ireturn
  w1 <*> w2 = WrapIx $ iap (unwrapIx w1) (unwrapIx w2)

instance IxAlternative m => Alternative (Ixed i i m) where
  empty = WrapIx iempty
  w1 <|> w2 = WrapIx $ ialt (unwrapIx w1) (unwrapIx w2)

instance IxMonad m => Monad (Ixed i i m) where
  return  = WrapIx . ireturn
  w >>= f = WrapIx $ ibind (unwrapIx w) (unwrapIx . f)

instance IxMonadZero m => MonadZero (Ixed i i m) where
  zero = WrapIx imzero

instance IxMonadAdd m => MonadAdd (Ixed i i m) where
  madd m1 m2 = WrapIx $ implus (unwrapIx m1) (unwrapIx m2)

instance IxMonadFix m => MonadFix (Ixed i i m) where
  mfix f = WrapIx $ imfix $ unwrapIx . f

instance IxCopointed w => Copointed (Ixed i i w) where
  extract = iextract . unwrapIx

instance IxCoapply w => Coapply (Ixed i i w) where
  coap wf wa = WrapIx $ icoap (unwrapIx wf) (unwrapIx wa)

instance IxCoalt w => Coalt (Ixed i i w) where
  coalt w = (WrapIx i1,WrapIx i2)
    where
    (i1,i2) = icoalt $ unwrapIx w
  cosome = map WrapIx . icosome . unwrapIx
  comany = map WrapIx . icomany . unwrapIx

{-
instance IxExtend w => Extend (Ixed i i w) where
  extend f wa = WrapIx $ iextend (f . unwrapIx) (unwrapIx wa)
-}

-- }}}
-}


{-
-- IxMonadTrans, ReaderT {{{

class IxMonadTrans t where
  ilift :: IxMonad m => m i j a -> t m (f i) (f j) a

class IxMonoid m where
  imempty  :: m i i
  imappend :: m i j -> m j k -> m i k

data IxReaderT
  (r  :: l_ -> l_ -> *)
  (im :: k_ -> k_ -> * -> *)
  (fi :: l_)
  (gj :: l_)
  (a  :: *) where
  IxReaderT
    :: (r (f i) (g j) -> im i j a)
    -> IxReaderT r im (f i) (g j) a

instance IxMonadTrans (IxReaderT r) where
  ilift m = IxReaderT $ \_rFiFj -> m

instance IxFunctor m => IxFunctor (IxReaderT r m) where
  imap f (IxReaderT m) = IxReaderT $ imap f . m

instance IxPointed m => IxPointed (IxReaderT r m) where
  ireturn = ilift . ireturn

instance IxPointed m => IxPointed (IxReaderT r m) where
  ireturn (a :: a) = IxReaderT f
    where
    f :: r (f i) (f i) -> m i i a
    f _rFiFi = m
    m :: m i i a
    m = ireturn a

instance IxApply m => IxApply (IxReaderT r m) where
  iap (IxReaderT (mf :: r (f i) (g j) -> m i j (a -> b)))
      (IxReaderT (ma :: r (g k) (h l) -> m k l a))
    = (IxReaderT m :: IxReaderT r m f h b)
    where
    m :: r (f i) (h k) -> m i k b
    m = undefined

ma :: r (j i2) (k2 j2) -> m i2 j2 a
mf :: r (i i1) (j j1) -> m i1 j1 (a -> b)
res :: 
iap :: IxReaderT r m i j (a -> b)
-> IxReaderT r m j k2 a -> IxReaderT r m i k2 b

instance IxBind m => IxBind (IxReaderT r m) where
  ibind (IxReaderT (m :: r (f i) (g j) -> m i j a)) f = undefined

-- }}}
-}


