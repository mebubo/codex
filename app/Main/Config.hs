module Main.Config where

import Control.Exception (catch)
import Data.Yaml
import System.Directory
import System.FilePath

import Codex

import qualified Distribution.Hackage.DB as DB
import qualified Distribution.Hackage.DB.Errors as Errors

data ConfigState = Ready | TaggerNotFound

getConfigPath :: IO FilePath
getConfigPath = do
  homedir <- getHomeDirectory
  return $ homedir </> ".codex"

checkConfig :: Codex -> IO ConfigState
checkConfig cx = do
  taggerExe <- findExecutable tagger
  return $ case taggerExe of
    Just _    -> Ready
    _         -> TaggerNotFound
  where
    tagger = head $ words (tagsCmd cx)

loadConfig :: IO Codex
loadConfig = decodeConfig >>= maybe defaultConfig return where
  defaultConfig = do
    hp <- DB.hackageTarball
      `catch` \Errors.NoHackageTarballFound -> error "Couldn't find a Hackage tarball"
    let cx = Codex True (dropFileName hp) defaultStackOpts (taggerCmd Hasktags) True defaultTagsFileName
    return cx

decodeConfig :: IO (Maybe Codex)
decodeConfig = do
  path  <- getConfigPath
  config path
  where
    config path = do
      res <- decodeFileEither path
      return $ eitherToMaybe res
    eitherToMaybe x = either (const Nothing) Just x

