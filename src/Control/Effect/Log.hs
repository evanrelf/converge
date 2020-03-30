{-# LANGUAGE LambdaCase #-}

module Control.Effect.Log
  ( Verbosity (..)
  , printVerbosity
  , Log (..)
  , log
  -- Re-exports
  , Algebra
  , Effect
  , Has
  , run
  )
where


import Control.Algebra (Algebra, Effect, HFunctor, Has, run, send)
import GHC.Generics (Generic1)


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


data Log m k
  = Log Verbosity Text (m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)


log :: Has Log sig m => Verbosity -> Text -> m ()
log severity message = send (Log severity message pass)
