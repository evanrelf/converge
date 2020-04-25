{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Log
  ( Verbosity (..)
  , printVerbosity
  , Log (..)
  , log
  , runLogIO
  )
where

import qualified Data.Text.IO as Text
import Polysemy


data Verbosity
  = Vomit
  | Debug
  | Info
  | Warn
  | Error
  deriving stock (Eq, Ord, Show)


printVerbosity :: IsString s => Verbosity -> s
printVerbosity = \case
  Vomit -> "[ VOMIT ]"
  Debug -> "[ DEBUG ]"
  Info  -> "[ INFO  ]"
  Warn  -> "[ WARN  ]"
  Error -> "[ ERROR ]"


data Log m a where
  Log :: Verbosity -> Text -> Log m ()

makeSem ''Log


runLogIO
  :: MonadIO m
  => Member (Embed m) r
  => Verbosity
  -> Sem (Log ': r) a
  -> Sem r a
runLogIO verbosity = interpret \case
  Log messageVerbosity message -> embed do
    when (messageVerbosity >= verbosity)
      (liftIO (Text.hPutStrLn stderr (printVerbosity messageVerbosity <> " " <> message)))
