{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}

module Run
  ( runWithArgs
  , Flags (..)
  , Key (..)
  , Name (..)
  , Builder (..)
  , M
  , returnM
  , failM
  , EndoM (..)
  , Failure (..)
  ) where

import ClassyPrelude
import Control.Lens (ifoldMap)

import Data.Char (isAlpha)
import System.Exit (exitFailure)

runWithArgs :: Flags r -> r -> (r -> IO a) -> IO a
runWithArgs fs r f = do
  as <- getArgs
  case parseFlags fs as r of
    Left err  -> do
      putStrLn $ "Failure: " ++ err
      exitFailure
    Right r' -> f r'



data Flags r = Flags
  { flags      :: Map Key (Builder r,Text)
  , withRest   :: [Text] -> M r
  , restFormat :: Text
  }

data Key = Key
  { fNames    :: Names
  , fPriority :: Int
  } deriving (Eq,Show)

data Names
  = ShortOnly ShortName
  | LongOnly  LongName
  | ShortLong ShortName LongName
  deriving (Eq,Ord,Show)

instance Ord Key where
  compare = comparing fPriority <> comparing fNames

type ShortName = Char

type LongName = Text

data Builder r
  = NoArg  (              M r)
  | OptArg (Maybe Text -> M r)
  | ReqArg (Text       -> M r)



flagUsage :: Flags r -> Text
flagUsage fs = ifoldMapOf (ifolded . to fst . to mkLine) 
  where
  mkLine flag desc = 
    where
    nm = case fNames flag of
      ShortOnly 

-- Parse Flags {{{ 

parseFlags :: Flags r -> [Text] -> r -> Either Text r
parseFlags fs as r = runM bldr r
  where
  bldr = ifoldMap build1 (flags fs) found <> withRest fs rest
  (found,rest) = sortFlags as
  build1 :: Key -> Builder r -> FoundFlags -> M r
  build1 (Key nms _) (b,_) ffs 
    | Just ma <- lookup nms ffs
    = case (b,ma)  of
      -- Good
      (NoArg  f,Nothing)  -> f
      (ReqArg f,Just a )  -> f a
      (OptArg f,_      )  -> f ma
      -- Bad
      (NoArg  _ ,Just a ) -> failM $ "Unexpected arg for flag " ++ nm' ++ ": " ++ a
      (ReqArg _ ,Nothing) -> failM $ "No argument given for flag " ++ nm'
    | otherwise = failM $ "Unrecognized flag: " ++ nm'
    where
    nms' = singleQuotes $ renderName nm

type M = EndoM Failure
runM :: M a -> a -> Either Text a
runM m a = checkFailure (runEndoM m a)

-- }}}

-- FoundFlags {{{

type FoundFlags = (Map Name (Maybe Text),[Text])

sortFlags :: [Text] -> FoundFlags
sortFlags [] = mempty
sortFlags (f:fs)
  | Just flag <- longFlag f
  = first (insertMap flag ma) $ sortFlags rest
  | Just flag <- shortFlag f
  = first (insertMap flag ma) $ sortFlags rest
  | otherwise
  = second (f:) $ sortFlags fs
  where
  (ma,rest) = hasArg fs

longFlag :: Text -> Maybe LongName
longFlag f
  | ('-':'-':c:_) <- toList f
  , isAlpha c
  = Just $ Long $ drop 2 f
  | otherwise = Nothing

shortFlag :: Text -> Maybe ShortName
shortFlag f
  | ['-',c] <- toList f
  , isAlpha c
  = Just c
  | otherwise
  = Nothing

hasArg :: [Text] -> (Maybe Text,[Text])
hasArg [] = (Nothing,[])
hasArg fs@(f:rest)
  | Just _ <- longFlag f
  = (Nothing,fs)
  | Just _ <- shortFlag f
  = (Nothing,fs)
  | otherwise
  = (Just f,rest)

-- }}}

-- Failure / EndoM / WrappedMonad {{{

newtype Failure a = Failure
  { checkFailure :: Either Text a
  } deriving (Eq,Ord,Show,Functor,Applicative)

instance Monad Failure where
  return = Failure . Right
  m >>= f = case checkFailure m of
    Left err -> Failure $ Left err
    Right a  -> f a
  fail = Failure . Left . fromString



newtype EndoM m a = EndoM
  { runEndoM :: a -> m a
  }

instance (Monad m) => Semigroup (EndoM m a) where
  m1 <> m2 = EndoM $ runEndoM m1 >=> runEndoM m2

instance (Monad m) => Monoid (EndoM m a) where
  mempty = EndoM return
  m1 `mappend`  m2 = EndoM $ runEndoM m1 >=> runEndoM m2



{-
newtype WrappedMonad m a = WrappedMonad
  { unwrapMonad :: m a
  } deriving (Functor,Applicative,Monad)

instance (Monad m, Monoid a) => Monoid (WrappedMonad m a) where
  mempty = return mempty
  m1 `mappend` m2 = do
    a <- m1
    b <- m2
    return $ a `mappend` b

instance (Monad m, Semigroup a) => Semigroup (WrappedMonad m a) where
  m1 <> m2 = do
    a <- m1
    b <- m2
    return $ a <> b
-}

-- }}}

-- Helpers {{{

renderName :: Name -> Text
renderName (Short c) = '-'  `cons` singleton c
renderName (Long t)  = "--" ++     t

returnM :: a -> M a
returnM = EndoM . const . return

failM :: Text -> M a
failM = EndoM . const . fail . toList

singleQuotes :: Text -> Text
singleQuotes = cons '\'' . flip snoc '\''

-- }}}

