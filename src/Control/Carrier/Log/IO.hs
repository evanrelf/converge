{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module Control.Carrier.Log.IO
  ( LogIOC (..)
  , runLog
  -- Re-exports
  , module Control.Effect.Log
  )
where


import Control.Algebra ((:+:) (..), alg, handleCoercible)
import qualified Data.Text.IO as Text

import Control.Effect.Log


runLog :: LogIOC m a -> m a
runLog (LogIOC m) = m


newtype LogIOC m a = LogIOC (m a)
  deriving stock Show
  deriving newtype (Functor, Applicative, Monad, MonadIO)


instance
  ( Algebra sig m
  , MonadIO m
  )
  => Algebra (Log :+: sig) (LogIOC m) where
  alg (R other) = LogIOC (alg (handleCoercible other))
  alg (L (Log severity message k)) = do
    let severityText =
          case severity of
            Debug -> "DEBUG"
            Info  -> "INFO "
            Warn  -> "WARN "
            Error -> "ERROR"
    liftIO (Text.hPutStrLn stderr ("[ " <> severityText <> " ] " <> message))
    k
