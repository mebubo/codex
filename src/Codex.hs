module Codex (Codex(..), defaultStackOpts, defaultTagsFileName, Verbosity, module Codex) where

import Control.Exception (try, SomeException)
import Control.Lens ((^.))
import Control.Lens.Review (bimap)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Maybe
import Data.List ((\\))
import Distribution.Package
import Distribution.Text
import Distribution.Verbosity
import Network.HTTP.Client (HttpException)
import System.Directory
import System.FilePath
import System.Process

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS

import Codex.Internal
import Codex.Project

replace :: String -> String -> String -> String
replace a b c = Text.unpack $ Text.replace (Text.pack a) (Text.pack b) (Text.pack c)

data Tagging = Tagged | Untagged
  deriving (Eq, Show)

fromBool :: Bool -> Tagging
fromBool True = Tagged
fromBool False = Untagged

data Status = Source Tagging | Archive | Remote
  deriving (Eq, Show)

type Action = ExceptT String IO

data Tagger = Ctags | Hasktags | HasktagsEmacs | HasktagsExtended
  deriving (Eq, Show, Read)

taggerCmd :: Tagger -> String
taggerCmd Ctags = "ctags --tag-relative=no --recurse -f \"$TAGS\" \"$SOURCES\""
taggerCmd Hasktags = "hasktags --ctags --follow-symlinks --output=\"$TAGS\" \"$SOURCES\""
taggerCmd HasktagsEmacs = "hasktags --etags --follow-symlinks --output=\"$TAGS\" \"$SOURCES\""
taggerCmd HasktagsExtended = "hasktags --ctags --follow-symlinks --extendedctag --output=\"$TAGS\" \"$SOURCES\""

taggerCmdRun :: Codex -> FilePath -> FilePath -> Action FilePath
taggerCmdRun cx sources tags' = do
  _ <- tryIO $ system command
  return tags' where
    command = replace "$SOURCES" sources $ replace "$TAGS" tags' $ tagsCmd cx

tryIO :: IO a -> Action a
tryIO io = do
  res <- liftIO $ (try :: IO a -> IO (Either SomeException a)) io
  either (throwE . show) return res

status :: FilePath -> PackageIdentifier -> Action Status
status root i = do
  sourcesExist <- tryIO . doesDirectoryExist $ packageSources root i
  archiveExist <- tryIO . doesFileExist $ packageArchive root i
  case (sourcesExist, archiveExist) of
    (True, _) -> fmap (Source . fromBool) (liftIO . doesFileExist $ packageTags root i)
    (_, True) -> return Archive
    (_, _)    -> return Remote

fetch :: WS.Session -> FilePath -> PackageIdentifier -> Action FilePath
fetch s root i = do
  bs <- tryIO $ do
    createDirectoryIfMissing True (packagePath root i)
    openLazyURI s url
  either throwE write bs where
      write bs = fmap (const archivePath) $ tryIO $ BS.writeFile archivePath bs
      archivePath = packageArchive root i
      url = packageUrl i

openLazyURI :: WS.Session -> String -> IO (Either String BS.ByteString)
openLazyURI s = fmap (bimap showHttpEx (^. W.responseBody)) . try . WS.get s where
  showHttpEx :: HttpException -> String
  showHttpEx = show

extract :: FilePath -> PackageIdentifier -> Action FilePath
extract root i = fmap (const path) . tryIO $ read' path (packageArchive root i) where
  read' dir tar = Tar.unpack dir . Tar.read . GZip.decompress =<< BS.readFile tar
  path = packagePath root i

tags :: Builder -> Codex -> PackageIdentifier -> Action FilePath
tags bldr cx i = taggerCmdRun cx sources tags' where
    sources = packageSources hp i
    tags' = packageTags hp i
    hp = hackagePathOf bldr cx

assembly :: Builder -> Codex -> [PackageIdentifier] -> [WorkspaceProject] -> FilePath -> Action FilePath
assembly bldr cx dependencies workspaceProjects o = do
  xs <- join . maybeToList <$> projects workspaceProjects
  tryIO $ mergeTags (fmap tags' dependencies ++ xs) o
  return o where
    projects [] = return Nothing
    projects xs = do
      tmp <- liftIO getTemporaryDirectory
      ys <- traverse (\wsp -> tags'' tmp wsp <* log' "Running tagger on " wsp) xs
      return $ Just ys where
        tags'' tmp (WorkspaceProject id' sources) = taggerCmdRun cx sources tags''' where
          tags''' = tmp </> concat [display id', ".tags"]
    mergeTags files' o' = do
      files'' <- filterM doesFileExist files'
      contents <- traverse (\f -> TLIO.readFile f <* log' "Merging " f) files''
      case files' \\ files'' of
        [] -> return ()
        xs -> do
          putStrLn "codex: *warning* the following tags files where missings during assembly:"
          mapM_ putStrLn xs
          return ()
      let xs = concat $ fmap TextL.lines contents
          ys = if sorted then (Set.toList . Set.fromList) xs else xs
      TLIO.writeFile o' $ TextL.unlines ys
    tags' = packageTags $ hackagePathOf bldr cx
    sorted = tagsFileSorted cx

log' :: (Show a, MonadIO m) => String -> a -> m ()
log' msg a = liftIO $ putStrLn $ msg ++ show a
