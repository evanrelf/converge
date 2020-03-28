module Control.Effect.Log
  ( Severity (..)
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


data Severity
  = Debug
  | Info
  | Warn
  | Error
  deriving stock (Eq, Ord, Show)


data Log m k
  = Log Severity Text (m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)


log :: Has Log sig m => Severity -> Text -> m ()
log severity message = send (Log severity message pass)
