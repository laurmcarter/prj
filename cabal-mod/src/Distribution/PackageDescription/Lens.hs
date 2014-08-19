{-# LANGUAGE TemplateHaskell #-}

module Distribution.PackageDescription.Lens where

import Control.Lens
import Data.Char

import Distribution.Compiler
import Distribution.License
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import Language.Haskell.Extension

makeLensesWith
  ( lensRules
  & lensField .~ (Just . ('_':) . (_head %~ toUpper))
  ) ''GenericPackageDescription

makeLensesWith
  ( lensRules
  & lensField .~ (Just . ('_':) . (_head %~ toUpper))
  ) ''CondTree

makePrisms ''Condition

makeLensesWith
  ( lensRules
  & lensField .~ (Just . ('_':) . (_head %~ toUpper))
  ) ''PackageDescription

makeLensesWith
  ( lensRules
  & lensField .~ (Just . ('_':) . (_head %~ toUpper))
  ) ''PackageIdentifier

makeIso ''PackageName

makePrisms ''License

makeLensesWith
  ( lensRules
  & lensField .~ (Just . ('_':) . (_head %~ toUpper))
  ) ''Version

makePrisms ''CompilerFlavor
makePrisms ''VersionRange

makeLensesWith
  ( lensRules
  & lensField .~ (Just . ('_':) . (_head %~ toUpper))
  ) ''SourceRepo

makePrisms ''RepoKind
makePrisms ''RepoType

makeLensesWith
  ( lensRules
  & lensField .~ (Just . ('_':) . (_head %~ toUpper))
  ) ''Dependency

makePrisms ''BuildType

makeLensesWith
  ( lensRules
  & lensField .~ (Just . ('_':) . (_head %~ toUpper))
  ) ''Library

makeLensesWith
  ( lensRules
  & lensField .~ (Just . ('_':) . (_head %~ toUpper))
  ) ''BuildInfo

makePrisms ''Language
makePrisms ''Extension
makePrisms ''KnownExtension

makeLensesWith
  ( lensRules
  & lensField .~ (Just . ('_':) . (_head %~ toUpper))
  ) ''Executable

makeLensesWith
  ( lensRules
  & lensField .~ (Just . ('_':) . (_head %~ toUpper))
  ) ''TestSuite

makePrisms ''TestSuiteInterface
makePrisms ''TestType

makeLensesWith
  ( lensRules
  & lensField .~ (Just . ('_':) . (_head %~ toUpper))
  ) ''Benchmark

makePrisms ''BenchmarkInterface
makePrisms ''BenchmarkType

