{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Distribution.PackageDescription.Modify where

import Control.Applicative
import Control.Monad
import Control.Lens
import Distribution.PackageDescription
import Distribution.PackageDescription.Lens
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint
import Distribution.ModuleName
import Distribution.Package
import Distribution.Verbosity

import Debug.Trace

onPackageDescription :: FilePath -> (PD -> IO PD) -> IO ()
onPackageDescription = flip $ over _PackageDescriptionIO . (=<<)

onGenericPackageDescription :: FilePath -> (GPD -> IO GPD) -> IO ()
onGenericPackageDescription = flip $ over _GenericPackageDescriptionIO . (=<<)

-- IOLens {{{

type IOLens a b = Lens a (IO ()) (IO b) (IO b)
type GPD = GenericPackageDescription
type PD  = PackageDescription

_GenericPackageDescriptionIO :: IOLens FilePath GPD
_GenericPackageDescriptionIO = _RW
  (readPackageDescription normal)
  (\a b -> do
    putStrLn $ "writing package desc '" ++ a ++ "'"
    print b
    putStrLn ""
    putStrLn $ showGenericPackageDescription b
    writeGenericPackageDescription a b)

_PackageDescriptionIO :: IOLens FilePath PD
_PackageDescriptionIO =
    _GenericPackageDescriptionIO 
  . _ap _PackageDescription

_FileIO :: IOLens FilePath String
_FileIO = _RW readFile writeFile

_RW ::
     (a -> IO b)
  -> (a -> b -> IO ())
  -> IOLens a b
_RW r w = _Mon $ lens r w

_ap :: forall m s t a b.  Applicative m
  => ALens s t a b
  -> Lens (m s) (m t) (m a) (m b)
_ap l = lens (fmap (^# l)) $ liftA2 $ flip $ storing l

_Mon :: Monad m => ALens s (m t) a b -> Lens s (m t) a (m b)
_Mon l = lens (^# l) $ \s -> (flip (storing l) s =<<)

-- }}}

