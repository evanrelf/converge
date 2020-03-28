{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}


module Control.Effect.Bread
  ( Bread
  , withCrumb
  , exit
  , handleExit
  , crumbs
  -- Re-exports
  , Algebra
  , Effect
  , Has
  , run
  )
where


import Control.Algebra ((:+:) (..), Algebra, Effect, Has, run)
import Control.Effect.Error (Error, catchError, throwError)
import Control.Effect.Reader (Reader, ask, local)


type Bread crumb exit = Reader [crumb] :+: Error exit


withCrumb :: Has (Bread crumb exit) sig m => crumb -> m a -> m a
withCrumb crumb = local (crumb :)


exit :: Has (Bread crumb exit) sig m => exit -> m a
exit = throwError


handleExit :: Has (Bread crumb exit) sig m => m a -> (exit -> m a) -> m a
handleExit = catchError


crumbs :: Has (Bread crumb exit) sig m => m [crumb]
crumbs = ask
