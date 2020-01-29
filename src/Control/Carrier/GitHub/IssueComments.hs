{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.GitHub.IssueComments
  ( IssueCommentsIOC (..)
  , runIssueCommentsIO
  )
where

import Control.Algebra

import Control.Effect.GitHub.IssueComments (IssueComments (..))


runIssueCommentsIO :: IssueCommentsIOC m a -> m a
runIssueCommentsIO (IssueCommentsIOC run) = run


newtype IssueCommentsIOC m a = IssueCommentsIOC (m a)
  deriving stock Show
  deriving newtype (Functor, Applicative, Monad)


instance Algebra sig m
      => Algebra (IssueComments :+: sig) (IssueCommentsIOC m) where
  alg (R other) = IssueCommentsIOC (alg (handleCoercible other))
  alg (L effect) = case effect of
    GetComment commentId k ->
      undefined

    GetComments issueNumber fetchCount k ->
      undefined

    CreateComment issueNumber body k ->
      undefined

    DeleteComment commentId k ->
      undefined

    EditComment commentId body k ->
      undefined
