{-# LANGUAGE NamedFieldPuns #-}

module Converge.Git
  ( Repo
  , clone
  , git
  , git_
  )
where

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
  path <- Temp.createTempDirectory parent "clone"
  git_ ["clone", repo, toText path]
  lock <- newMVar ()
  pure Repo{path, lock}


git :: MonadIO m => [Text] -> m (ExitCode, Text, Text)
git args = liftIO do
  (exitCode, stdoutString, stderrString) <-
    Process.readProcessWithExitCode "git" (toString <$> args) mempty
  pure (exitCode, toText stdoutString, toText stderrString)


git_ :: MonadIO m => [Text] -> m ()
git_ = void . git
