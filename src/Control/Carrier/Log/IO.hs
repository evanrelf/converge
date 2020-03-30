{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Control.Carrier.Log.IO
  ( LogIOC (..)
  , runLog
  -- Re-exports
  , module Control.Effect.Log
  )
where


import Control.Algebra ((:+:) (..), alg, handleCoercible)
import Control.Carrier.Reader (Reader, ask, runReader)
import qualified Data.Text.IO as Text

import Control.Effect.Log


runLog :: MonadIO m => Verbosity -> _m a -> m a
runLog verbosity (LogIOC m) = runReader verbosity m


newtype LogIOC m a = LogIOC (m a)
  deriving stock Show
  deriving newtype (Functor, Applicative, Monad, MonadIO)


instance
  ( Algebra sig m
  , Has (Reader Verbosity) sig m
  , MonadIO m
  )
  => Algebra (Log :+: sig) (LogIOC m) where
  alg (R other) = LogIOC (alg (handleCoercible other))
  alg (L (Log messageVerbosity message k)) = do
    verbosity <- ask
    when (messageVerbosity >= verbosity)
      (liftIO (Text.hPutStrLn stderr (printVerbosity messageVerbosity <> " " <> message)))
    k
