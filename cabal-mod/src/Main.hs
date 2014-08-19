{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Distribution.PackageDescription.Lens
import Distribution.PackageDescription.Modify

import Distribution.ModuleName hiding (main)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint
import Distribution.Text
import Distribution.Version
import Distribution.Verbosity
import Control.Lens hiding (argument,(<.>))
import Options.Applicative hiding ((&))
import System.Directory
import System.FilePath
import System.FilePath.Glob

import Control.Monad
import Data.Maybe
import Data.Monoid

main :: IO ()
main = do
  cfg <- execParser
    $ info parseConfig
    $ fullDesc
   <> progDesc "Modify a project's .cabal file"
   <> header   "cabal-mod  -  a .cabal file modification utility"
  let runCommand = case cfg of
        DependencyCmd x -> cmdType addDependency (const remDependency) dspDependencies x
        ExecutableCmd x -> cmdType addExecutable (const remExecutable) dspExecutables  x
        ModuleCmd     x -> cmdType addModule            remModule      dspModules      x
  cbl <- findCabalFile
  readPackageDescription normal cbl
    >>= runCommand
    >>= writeGenericPackageDescription cbl

-- Types {{{

data Task
  = DependencyCmd DepOpt
  | ExecutableCmd ExeOpt
  | ModuleCmd     ModOpt
  deriving (Eq,Show)

type DepOpt = Type  Dependency       PackageName
type ExeOpt = Type (String,FilePath) String
type ModOpt = Type  ModuleName       ModuleName  

data a :+: b
  = L a
  | R b
  deriving (Eq,Show)
infixr 5 :+:

{-
newtype Add a = Add { add :: a } deriving (Eq,Show)
newtype Rem a = Rem { rem :: a } deriving (Eq,Show)
newtype Dsp a = Dsp { dsp :: a } deriving (Eq,Show)
-}

data Type a b
  = Add a
  | Rem
    Bool -- ^ Remove (True) or Disable (False)
    b
  | Disp
  deriving (Eq,Show)

cmdType :: (a -> c) -> (Bool -> b -> c) -> c -> Type a b -> c
cmdType a r d = \case
  Add   x -> a x
  Rem b x -> r b x
  Disp    -> d

-- }}}

-- Parser {{{

parseConfig :: Parser Task
parseConfig = helper_ $ subparser
  .$. command "dep"
   $.. info parseDep
   $.. fullDesc
    <> header "Add or Remove a Dependency"
  .+. command "exe"
   $.. info parseExe
   $.. fullDesc
    <> header "Add or Remove an Executable"
  .+. command "mod"
   $.. info parseMod
   $.. fullDesc
    <> header "Add or Remove a Module"

parseDep :: Parser Task
parseDep = helper_ $ DependencyCmd
  <$> subparser
  .$. command "add"
   $.. info parseAddDep
   $.. fullDesc
    <> header "Add a Dependency"
  .+. command "rem"
   $.. info parseRemDep
   $.. fullDesc
    <> header "Remove a Dependency"
  .+. command "dsp"
   $.. info parseDspDep
   $.. fullDesc
    <> header "Display Dependencies"
  .+. help "Possible commands: add | rem | dsp"

parseExe :: Parser Task
parseExe = helper_ $ ExecutableCmd
  <$> subparser
  .$. command "add"
   $.. info parseAddExe
   $.. fullDesc
    <> header "Add an Executable"
  .+. command "rem"
   $.. info parseRemExe
   $.. fullDesc
    <> header "Remove an Executable"
  .+. command "dsp"
   $.. info parseDspExe
   $.. fullDesc
    <> header "Display Executables"
  .+. help "Possible commands: add | rem | dsp"

parseMod :: Parser Task
parseMod = helper_ $ ModuleCmd
  <$> subparser
  .$. command "add"
   $.. info parseAddMod
   $.. fullDesc
    <> header "Add a Module"
  .+. command "rem"
   $.. info parseRemMod
   $.. fullDesc
    <> header "Remove a Module"
  .+. command "dsp"
   $.. info parseDspMod
   $.. fullDesc
    <> header "Display Modules"
  .+. help "Possible commands: add | rem | dsp"

parseAddDep :: Parser DepOpt
parseAddDep = helper_ $ Add
  <$> argument simpleParse
  .$. metavar "DEPENDENCY"
  .+. help "Add a dependency to library and executables"

parseRemDep :: Parser DepOpt
parseRemDep = helper_ $ Rem True
  <$> argument simpleParse
  .$. metavar "DEPENDENCY"
  .+. help "Remove a dependency from library and executables"

parseDspDep :: Parser DepOpt
parseDspDep = pure Disp

parseAddExe :: Parser ExeOpt
parseAddExe = helper_ $ Add
  <$> ((,)
  <$> argument Just
  .$. metavar "EXE-NAME"
  <*> strOption
  .$. value "Main.hs"
  .+. metavar "path/to/main.hs"
  .+. showDefaultWith id
  .+. help "Add an executable to project"
  )

parseRemExe :: Parser ExeOpt
parseRemExe = helper_ $ Rem True
  <$> argument Just
  .$. metavar "EXE-NAME"
  .+. help "Remove an executable from project"

parseDspExe :: Parser ExeOpt
parseDspExe = pure Disp

parseAddMod :: Parser ModOpt
parseAddMod = helper_ $ Add
  <$> argument simpleParse
  .$. metavar "MODULE"
  .+. help "Add an exposed module to library"

parseRemMod :: Parser ModOpt
parseRemMod = helper_ $ Rem
  <$> switch
  .$. short 'd'
  .+. long "disable"
  .+. help "Disable, don't remove"
  <*> argument simpleParse
  .$. metavar "MODULE"
  .+. help "Remove an exposed module from library"

parseDspMod :: Parser ModOpt
parseDspMod = pure Disp

-- }}}

-- Dependency {{{

addDependency :: Dependency -> GPD -> IO GPD
addDependency d pkg = do
  putStrLn $ "Adding Dependency '" ++ display d ++ "'"
  return $ pkg
    &  _CondLibrary._Just._CondTreeConstraints
    %~ (d:)
    &  _CondExecutables.each.mapped._CondTreeConstraints
    %~ (d:)

remDependency :: PackageName -> GPD -> IO GPD
remDependency pn pkg = do
  putStrLn $ "Removing Dependency '" ++ display pn ++ "'"
  return $ pkg
    &  _CondLibrary._Just._CondTreeConstraints
    %~ filter (\(Dependency pn' _) -> pn /= pn')
    &  _CondExecutables.each.mapped._CondTreeConstraints
    %~ filter (\(Dependency pn' _) -> pn /= pn')

dspDependencies :: GPD -> IO GPD
dspDependencies pkg = do
  putStrLn $ "Current Dependencies:"
  let mds = pkg ^? _CondLibrary._Just._CondTreeConstraints
  case mds of
    Just ds -> forM_ ds $ putStrLn . ("  " ++) . display
    _       -> putStrLn "  None."
  return pkg

-- }}}

-- Executable {{{

addExecutable :: (String,FilePath) -> GPD -> IO GPD
addExecutable (e,p) pkg = do
  putStrLn $ "Adding Executable '" ++ e ++ "'"
  let pth = srcPrefix pkg </> p
      dir = takeDirectory pth
  putStrLn $ "Creating File '" ++ pth ++ "'"
  createDirectoryIfMissing True dir
  writeFile pth $ unlines
    [ ""
    , "module Main where"
    , ""
    ]
  let md      = pkg ^? _CondLibrary._Just._CondTreeConstraints
      msrc    = pkg ^? _CondLibrary._Just._CondTreeData._LibBuildInfo._HsSourceDirs
      pkgNm   = pkg^._PackageDescription._Package._PkgName
      thisDep = mkDep pkgNm
  return $ pkg
    & _CondExecutables
    %~ ((:)
      ( e
      , CondNode
        ( emptyExecutable
        & _ModulePath .~ p
        & _BuildInfo._HsSourceDirs .~ fromMaybe ["."] msrc
        )
        (maybe [baseDep,thisDep] (thisDep:) md)
        []
      ))

remExecutable :: String -> GPD -> IO GPD
remExecutable e pkg = do
  putStrLn $ "Removing Executable '" ++ e ++ "'"
  return $ pkg
    & _CondExecutables
    %~ filter (\(e',_) -> e /= e')

dspExecutables :: GPD -> IO GPD
dspExecutables pkg = do
  putStrLn $ "Current Executables:"
  let es = pkg ^. _CondExecutables
  forM es $ \(nm,e) -> putStrLn $ "  " ++ nm ++ " : " ++ e^._CondTreeData._ModulePath
  return pkg

-- }}}

-- Module {{{

addModule :: ModuleName -> GPD -> IO GPD
addModule m pkg = do
  putStrLn $ "Adding Module '" ++ display m ++ "'"
  let p   = srcPrefix pkg </> toFilePath m <.> "hs"
      dir = takeDirectory p
  unlessM (doesFileExist p) $ do
    createDirectoryIfMissing True dir
    putStrLn $ "Creating File '" ++ p ++ "'"
    writeFile p $ unlines
      [ ""
      , "module " ++ display m ++ " where"
      , ""
      ]
  return $ pkg
    & _CondLibrary._Just._CondTreeData._ExposedModules
    %~ (m:)

remModule :: Bool -> ModuleName -> GPD -> IO GPD
remModule hard m pkg = do
  putStrLn $ msg ++ " Module '" ++ display m ++ "'"
  let p   = srcPrefix pkg </> toFilePath m <.> "hs"
      dir = takeDirectory p
  unless hard $ do
    putStrLn $ "Deleting File '" ++ p ++ "'"
    condM (doesFileExist p)
      ( removeFile p
     >> removeDirectoryIfEmpty dir)
      ( fail $ "No such file '" ++ p ++ "'")
  return $ pkg
    & _CondLibrary._Just._CondTreeData._ExposedModules
    %~ filter (/= m)
  where
  msg = if hard then "Removing" else "Disabling"

dspModules :: GPD -> IO GPD
dspModules pkg = do
  putStrLn $ "Exposed Modules:"
  let mms = pkg ^? _CondLibrary._Just._CondTreeData._ExposedModules
  case mms of
    Just ms -> forM_ ms $ putStrLn . ("  " ++) . display
    _       -> putStrLn "  None."
  return pkg

-- }}}

-- Helpers {{{

helper_ :: Parser a -> Parser a
helper_ = (helper <*>)

srcPrefix :: GPD -> FilePath
srcPrefix pkg = case src of
  Just []             -> "."
  Just [p]            -> p
  Just ps@(p:_)
    | "src" `elem` ps -> "src"
    | otherwise       -> p
  _                   -> "."
  where
  src = pkg ^? _CondLibrary._Just._CondTreeData._LibBuildInfo._HsSourceDirs

findCabalFile :: IO FilePath
findCabalFile = do
  fs <- glob "*.cabal"
  case fs of
    [f] -> return f
    []  -> fail "No .cabal file found"
    _   -> fail "Multiple .cabal files found"

removeDirectoryIfEmpty :: FilePath -> IO ()
removeDirectoryIfEmpty p = do
  whenM (doesDirectoryExist p)
    $ whenM (null <$> getDirectoryContents p) $ do
      removeDirectory p
      let ps = splitDirectories p
      case ps of
        [] -> return ()
        _  -> removeDirectoryIfEmpty $ joinPath $ init ps

mkDep :: PackageName -> Dependency
mkDep n = Dependency n anyVersion

baseDep :: Dependency
baseDep = mkDep $ PackageName "base"

(.$.) :: (a -> b) -> a -> b
f .$. x = f x
infixr 5 .$.

($..) :: (a -> b) -> a -> b
f $.. x = f x
infixr 6 $..

(.+.) :: Monoid m => m -> m -> m
(.+.) = mappend
infixr 5 .+.

whenM :: Monad m => m Bool -> m () -> m ()
whenM b m = b >>= flip when m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b m = b >>= flip unless m

condM :: Monad m => m Bool -> m a -> m a -> m a
condM t c a = t >>= \b -> if b then c else a

-- }}}

