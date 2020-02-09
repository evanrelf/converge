{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Control.Carrier.GitHub.IssueComments.IO
  ( IssueCommentsIOC (..)
  , runIssueCommentsIO
  -- Re-exports
  , module Control.Effect.GitHub.IssueComments
  )
where


import Control.Algebra ((:+:) (..), alg, handleCoercible)
import Control.Carrier.Lift (Lift, runM, sendM)
import Control.Carrier.Reader (Reader, ask, runReader)
import Control.Carrier.Throw.Either (Throw, runThrow, throwError)
import GitHub.Data (Auth, Error, Name, Owner, Repo)
import GitHub.Endpoints.Issues.Comments
  ( commentR
  , commentsR
  , createCommentR
  , deleteCommentR
  , editCommentR
  )
import GitHub.Request (github)

import Control.Effect.GitHub.IssueComments


runIssueCommentsIO
  :: Auth
  -> Name Owner
  -> Name Repo
  -> _m a
  -> IO (Either Error a)
runIssueCommentsIO auth owner repo
  = runM
  . runThrow
  . runReader repo
  . runReader owner
  . runReader auth
  . runIssueCommentsIOC


newtype IssueCommentsIOC m a = IssueCommentsIOC { runIssueCommentsIOC :: m a }
  deriving stock Show
  deriving newtype (Functor, Applicative, Monad)


instance
  ( Algebra sig m
  , Has (Reader Auth) sig m
  , Has (Reader (Name Owner)) sig m
  , Has (Reader (Name Repo)) sig m
  , Has (Throw Error) sig m
  , Has (Lift IO) sig m
  )
  => Algebra (IssueComments :+: sig) (IssueCommentsIOC m) where
  alg (R other) = IssueCommentsIOC (alg (handleCoercible other))
  alg (L effect) = do
    auth <- ask @Auth
    owner <- ask @(Name Owner)
    repo <- ask @(Name Repo)

    case effect of
      GetComment commentId k -> do
        result <- sendM @IO (github auth (commentR owner repo commentId))
        case result of
          Left err -> throwError @Error err
          Right issueComment -> k issueComment

      GetComments issueNumber fetchCount k -> do
        result <- sendM @IO
          (github auth (commentsR owner repo issueNumber fetchCount))
        case result of
          Left err -> throwError @Error err
          Right issueComments -> k issueComments

      CreateComment issueNumber body k -> do
        result <- sendM @IO
          (github auth (createCommentR owner repo issueNumber body))
        case result of
          Left err -> throwError @Error err
          Right comment -> k comment

      DeleteComment commentId k -> do
        result <- sendM @IO (github auth (deleteCommentR owner repo commentId))
        case result of
          Left err -> throwError @Error err
          Right () -> k

      EditComment commentId body k -> do
        result <- sendM @IO
          (github auth (editCommentR owner repo commentId body))
        case result of
          Left err -> throwError @Error err
          Right comment -> k comment
