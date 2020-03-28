{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module Control.Carrier.Bread
  ( BreadC
  , runBread
  -- Re-exports
  , module Control.Effect.Bread
  )
where


import Control.Algebra ((:+:) (..), alg, handleCoercible)

import Control.Effect.Bread


runBread :: BreadC crumb exit m a -> m a
runBread (BreadC m) = m


newtype BreadC crumb exit m a = BreadC (m a)
  deriving stock Show
  deriving newtype (Functor, Applicative, Monad)


instance
  ( Algebra sig m
  )
  => Algebra (Bread crumb exit :+: sig) (BreadC crumb exit m) where
  alg (R other) = undefined
  alg (L effect) = case effect of
    _ -> undefined
