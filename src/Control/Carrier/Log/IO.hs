{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Control.Carrier.Log.IO
  ( LogIOC (..)
  , runLogIO
  -- Re-exports
  , module Control.Effect.Log
  )
where


import Control.Algebra ((:+:) (..), alg, handleCoercible)
import qualified Data.Text.IO as Text

import Control.Effect.Log


runLogIO :: LogIOC m a -> m a
runLogIO (LogIOC m) = m


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
            Info ->
              "[ INFO  ]"
            Warn ->
              "[ WARN  ]"
            Error ->
              "[ ERROR ]"
    liftIO (Text.hPutStrLn stderr (severityText <> " " <> message))
    k
