module Main (main) where

import Control.Arrow
import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Either
import Data.List
import qualified Distribution.Hackage.DB as DB
import Distribution.Text
import Network.Socket (withSocketsDo)
import Network.Wreq.Session (withSession)
import Paths_codex (version)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process (shell, readCreateProcessWithExitCode)

import Codex
import Codex.Project
import Codex.Internal (Builder(..), hackagePathOf, readStackPath)
import Main.Config

retrying :: Int -> IO (Either a b) -> IO (Either [a] b)
retrying n x = retrying' n $ fmap (left (:[])) x where
  retrying' 0  x' = x'
  retrying' n' x' = retrying' (n' - 1) $ x' >>= \res -> case res of
    Left ls -> fmap (left (++ ls)) x'
    Right r -> return $ Right r

cleanCache :: (Builder, Codex) -> IO ()
cleanCache (bldr, cx) = do
  xs <- listDirectory' hp
  ys <- builderOp bldr =<< traverseDirectories xs
  _  <- removeTagFiles $ concat ys
  return ()
  where
    hp = hackagePath cx
    safe = (try :: IO a -> IO (Either SomeException a))
    listDirectory' fp = do
      xs <- getDirectoryContents fp
      return . fmap (fp </>) $ filter (not . isPrefixOf ".") xs
    removeTagFiles = traverse (\a -> ((safe . removeFile) a >> log' "Removing " a)) . fmap (</> "tags")
    traverseDirectories = fmap rights . traverse (safe . listDirectory')
    builderOp (Stack _) = traverseDirectories . concat
    builderOp Cabal = return

update :: Codex -> Builder -> IO ()
update cx bldr = do
  (mpid, dependencies, workspaceProjects') <- case bldr of
       Cabal -> do
         tb <- DB.hackageTarball
         resolveCurrentProjectDependencies bldr tb
       Stack _ -> resolveCurrentProjectDependencies bldr $ hackagePath cx

  let workspaceProjects = if currentProjectIncluded cx
        then workspaceProjects'
        else filter (("." /=) . workspaceProjectPath) workspaceProjects'
  fileExist <- doesFileExist tagsFile
  when fileExist $ removeFile tagsFile
  putStrLn ("Updating: " ++ displayPackages mpid workspaceProjects)
  results <- withSession $ \s -> do
    traverse (retrying 3 . runExceptT . getTags s) dependencies
  _       <- traverse print . concat $ lefts results
  res     <- runExceptT $ assembly bldr cx dependencies workspaceProjects tagsFile
  case res of
    Left e -> do
      print e
      exitFailure
    Right _ -> pure ()
  where
    tagsFile = tagsFileName cx
    hp = hackagePathOf bldr cx
    getTags s i = status hp i >>= \x -> case x of
      Source Tagged   -> log' "Already tagged: " i >> return ()
      Source Untagged -> tags bldr cx i >> log' "Tagged " i >> getTags s i
      Archive         -> extract hp i >> log' "Unpacked " i >> getTags s i
      Remote          -> liftIO $ either ignore return <=< runExceptT $ fetch s hp i >> log' "Downloaded " i >> getTags s i
        where
          ignore msg = do
            putStrLn $ concat ["codex: *warning* unable to fetch an archive for ", display i]
            putStrLn msg
            return ()
    displayPackages mpid workspaceProjects =
      case mpid of
        Just p -> display p
        Nothing ->
          unwords (fmap (display . workspaceProjectIdentifier) workspaceProjects)

help :: IO ()
help = putStrLn $
  unlines [ "Usage: codex [update] [cache clean] [set tagger [hasktags|ctags]] [set format [vim|emacs|sublime]]"
          , "             [--help]"
          , "             [--version]"
          , ""
          , " update                Synchronize the tags file in the current cabal project directory"
          , " cache clean           Remove all `tags` file from the local hackage cache]"
          , ""
          , "Note: codex will browse the parent directory for cabal projects and use them as dependency over hackage when possible." ]

main :: IO ()
main = withSocketsDo $ do
  cx    <- loadConfig
  args  <- getArgs
  run cx args where
    run cx ["cache", "clean"] = toBuilderConfig cx >>= cleanCache
    run cx ["update"]             = withConfig cx update
    run _  ["--version"] = putStrLn $ concat ["codex: ", display version]
    run _  ["--help"] = help
    run _  []         = help
    run _  args       = fail' $ concat ["codex: '", intercalate " " args,"' is not a codex command. See 'codex --help'."]

    toBuilderConfig cx' = checkConfig cx' >>= \state -> case state of
      TaggerNotFound  -> fail' $ "codex: tagger not found."
      Ready           -> do
        stackFileExists <- doesFileExist $ "." </> "stack.yaml"
        stackWorkExists <- doesDirectoryExist $ "." </> ".stack-work"
        if stackFileExists && stackWorkExists then do
            (ec, _, _) <- readCreateProcessWithExitCode (shell "which stack") ""
            case ec of
                ExitSuccess -> do
                    let opts = stackOpts cx'
                    globalPath <- readStackPath opts "stack-root"
                    binPath <- readStackPath opts "bin-path"
                    path <- getEnv "PATH"
                    setEnv "PATH" $ concat [path, ":", binPath]
                    return (Stack opts, cx' { hackagePath = globalPath </> "indices" </> "Hackage" })
                _ ->
                    return (Cabal, cx')
        else return (Cabal, cx')

    withConfig cx' f = do
      (bldr, cx) <- toBuilderConfig cx'
      cleanCache (bldr, cx)
      res <- f cx bldr
      return res

    fail' msg = do
      putStrLn $ msg
      exitWith (ExitFailure 1)
