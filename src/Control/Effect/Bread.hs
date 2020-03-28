{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}


module Control.Effect.Bread
  ( Bread (..)
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


-- Inspired by https://github.com/parsonsmatt/garlic-bread
-- TODO: Try re-implementing this as (Reader :+: Throw) instead


import Control.Algebra (Algebra, Effect (..), HFunctor (..), Has, run, send)
import GHC.Generics (Generic1)


data Bread crumb exit m k
  = forall a. WithCrumb crumb (m a) (a -> m k)
  | forall a. Exit exit (a -> m k)
  | forall a. HandleExit (m a) (exit -> m a) (a -> m k)
  | Crumbs ([crumb] -> m k)


deriving instance Functor m => Functor (Bread crumb exit m)


instance HFunctor (Bread crumb exit) where
  hmap f = \case
    WithCrumb crumb action k ->
      WithCrumb crumb (f action) (f . k)

    Exit exit k ->
      Exit exit (f . k)

    HandleExit action handler k ->
      HandleExit (f action) (f . handler) (f . k)

    Crumbs k ->
      Crumbs (f . k)


instance Effect (Bread crumb exit) where
  thread ctx handler = \case
    WithCrumb crumb action k ->
      WithCrumb crumb (handler (action <$ ctx)) (handler . fmap k)

    Exit err k ->
      Exit err (handler . (<$ ctx) . k)

    HandleExit action exitHandler k ->
      -- TODO: Finish this
      HandleExit (handler undefined) (handler . undefined) (handler . undefined)

    Crumbs k ->
      Crumbs (handler . (<$ ctx) . k)


withCrumb
  :: forall crumb exit sig m a
   . Has (Bread crumb exit) sig m
  => crumb
  -> m a
  -> m a
withCrumb crumb action = send (WithCrumb @_ @exit crumb action pure)


exit :: forall crumb exit sig m a. Has (Bread crumb exit) sig m => exit -> m a
exit exit' = send (Exit @crumb exit' pure)


handleExit
  :: forall crumb exit sig m a
   . Has (Bread crumb exit) sig m
  => m a
  -> (exit -> m a)
  -> m a
handleExit action handler = send (HandleExit @crumb action handler pure)


crumbs :: forall crumb exit sig m. Has (Bread crumb exit) sig m => m [crumb]
crumbs = send (Crumbs @_ @exit pure)
