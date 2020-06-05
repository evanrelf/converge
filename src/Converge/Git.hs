module Converge.Git
  ( git
  , git_
  )
where

import System.Exit (ExitCode)
import qualified System.Process as Process


git :: MonadIO m => [Text] -> m (ExitCode, Text, Text)
git args = liftIO do
  (exitCode, stdoutString, stderrString) <-
    Process.readProcessWithExitCode "git" (toString <$> args) mempty
  pure (exitCode, toText stdoutString, toText stderrString)


git_ :: MonadIO m => [Text] -> m ()
git_ = void . git
