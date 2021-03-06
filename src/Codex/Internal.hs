{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Codex.Internal where

import Data.Char (isSpace)
import Data.Yaml
import Data.Maybe (mapMaybe)
import Data.Version (versionBranch, Version, parseVersion)
import Distribution.Package
import Distribution.Text
import GHC.Generics
import System.FilePath
import System.Process (shell, readCreateProcess)
import Text.ParserCombinators.ReadP (readP_to_S)

import qualified Data.List as L

defaultStackOpts :: FilePath
defaultStackOpts = ""

defaultTagsFileName :: FilePath
defaultTagsFileName = "codex.tags"

data Builder = Cabal | Stack String

data Codex = Codex
  { currentProjectIncluded :: Bool
  , hackagePath :: FilePath
  , stackOpts :: String
  , tagsCmd :: String
  , tagsFileSorted :: Bool
  , tagsFileName :: FilePath }
    deriving Show

deriving instance Generic Codex
instance ToJSON Codex
instance FromJSON Codex

hackagePathOf :: Builder -> Codex -> FilePath
hackagePathOf Cabal     cx = hackagePath cx
hackagePathOf (Stack _) cx = hackagePath cx </> "packages"

packagePath :: FilePath -> PackageIdentifier -> FilePath
packagePath root i = root </> relativePath i where
  relativePath _ = name </> version where
    name = display $ pkgName i
    version = display $ pkgVersion i

packageArchive :: FilePath -> PackageIdentifier -> FilePath
packageArchive root i = packagePath root i </> name where
  name = concat [display $ pkgName i, "-", display $ pkgVersion i, ".tar.gz"]

packageSources :: FilePath -> PackageIdentifier -> FilePath
packageSources root i = packagePath root i </> name where
  name = concat [display $ pkgName i, "-", display $ pkgVersion i]

packageTags :: FilePath -> PackageIdentifier -> FilePath
packageTags root i = packagePath root i </> "tags"

packageUrl :: PackageIdentifier -> String
packageUrl i = concat ["http://hackage.haskell.org/package/", path] where
  path = concat [name, "/", name, ".tar.gz"]
  name = concat [display $ pkgName i, "-", display $ pkgVersion i]

removePrefix :: String -> String -> Maybe String
removePrefix prefix str =
  if prefix `L.isPrefixOf` trim str
    then Just $ trim $ L.drop (length prefix) $ trim str
    else Nothing
 where
  trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

readStackPath :: String -> String -> IO String
readStackPath opts id' = do
  let cmd = concat ["stack ", opts, " path --", id']
  s <- readCreateProcess (shell cmd) ""
  return $ init s

stackListDependencies :: String -> String -> IO [PackageIdentifier]
stackListDependencies opts pname = do
  s <- readCreateProcess (shell cmd) ""
  return $ mapMaybe parsePackageIdentifier $ lines s
  where
    cmd = concat ["stack ", opts, " ls dependencies ", pname]
    parsePackageIdentifier line =
      let line' = map (\c -> if c == ' ' then '-' else c)
                      line
       in  simpleParse line'
