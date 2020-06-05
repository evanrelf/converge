{-# LANGUAGE NamedFieldPuns #-}

module Converge.Git
  ( Repo
  , clone
  , withRepo
  , withRepoDisposable
  , git
  , git_
  )
where

import Control.Concurrent.MVar (withMVar)
import qualified System.Directory as Directory
import System.Exit (ExitCode)
import qualified System.IO.Temp as Temp
import qualified System.Process as Process


data Repo = Repo
  { path :: FilePath
  , lock :: MVar ()
  }


clone :: MonadIO m => Text -> m Repo
clone repo = liftIO do
  parent <- Temp.getCanonicalTemporaryDirectory
  path <- Temp.createTempDirectory parent "converge-repo"
  git_ ["clone", repo, toText path]
  lock <- newMVar ()
  pure Repo{path, lock}


withRepo :: Repo -> IO a -> IO a
withRepo Repo{path, lock} action =
  withMVar lock \_ -> Directory.withCurrentDirectory path action


withRepoDisposable :: Repo -> IO a -> IO a
withRepoDisposable Repo{path, lock} action =
  Temp.withSystemTempDirectory "converge-repo-disposable" \disposablePath -> do
    withMVar lock \_ -> git_ ["clone", toText path, toText disposablePath]
    Directory.withCurrentDirectory disposablePath action


git :: MonadIO m => [Text] -> m (ExitCode, Text, Text)
git args = liftIO do
  (exitCode, stdoutString, stderrString) <-
    Process.readProcessWithExitCode "git" (toString <$> args) mempty
  pure (exitCode, toText stdoutString, toText stderrString)


git_ :: MonadIO m => [Text] -> m ()
git_ = void . git
