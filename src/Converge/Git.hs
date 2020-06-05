module Converge.Git
  ( git
  , git_
  )
where

import Optics (_2, _3, over)
import System.Exit (ExitCode)
import qualified System.Process as Process


git :: MonadIO m => [Text] -> m (ExitCode, Text, Text)
git args = liftIO do
  Process.readProcessWithExitCode "git" (toString <$> args) mempty
    <&> over _2 toText
    <&> over _3 toText


git_ :: MonadIO m => [Text] -> m ()
git_ = void . git
