{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Control.Carrier.Bread
  ( BreadC
  , runBread
  -- Re-exports
  , module Control.Effect.Bread
  )
where


import Control.Algebra ((:+:) (..), alg, handleCoercible)
import Control.Carrier.Error.Either (Error, runError)
import Control.Carrier.Reader (Reader, runReader)

import Control.Effect.Bread


runBread :: _m a -> m (Either ([crumb], exit) a)
runBread (BreadC m)
  = runError
  . runReader []
  $ m


newtype BreadC crumb exit m a = BreadC (m a)
  deriving stock Show
  deriving newtype (Functor, Applicative, Monad)


instance
  ( Algebra sig m
  , Has (Error exit) sig m
  , Has (Reader [crumb]) sig m
  )
  => Algebra (Bread crumb exit :+: sig) (BreadC crumb exit m) where
  alg (R other) = BreadC (alg (handleCoercible other))
  alg (L effect) = case effect of
    _ -> undefined
