{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module GitHub.Carrier.PullRequest.Comments.IO
  ( PullRequestCommentsIOC (..)
  , runPullRequestCommentsIO
  -- Re-exports
  , module GitHub.Effect.PullRequest.Comments
  )
where


import Control.Algebra ((:+:) (..), alg, handleCoercible)
import Control.Carrier.Reader (Reader, ask, runReader)
import Control.Carrier.Throw.Either (Throw, runThrow, throwError)
import GitHub.Data (Auth, Error, Name, Owner, Repo)
import GitHub.Endpoints.PullRequests.Comments
  ( pullRequestCommentR
  , pullRequestCommentsR
  , createPullCommentR
  )
import GitHub.Request (github)

import GitHub.Effect.PullRequest.Comments


runPullRequestCommentsIO
  :: Auth
  -> Name Owner
  -> Name Repo
  -> _m a
  -> IO (Either Error a)
runPullRequestCommentsIO auth owner repo (PullRequestCommentsIOC m)
  = runThrow
  . runReader repo
  . runReader owner
  . runReader auth
  $ m


newtype PullRequestCommentsIOC m a
  = PullRequestCommentsIOC (m a)
  deriving stock Show
  deriving newtype (Functor, Applicative, Monad, MonadIO)


instance
  ( Algebra sig m
  , Has (Reader Auth) sig m
  , Has (Reader (Name Owner)) sig m
  , Has (Reader (Name Repo)) sig m
  , Has (Throw Error) sig m
  , MonadIO m
  )
  => Algebra (PullRequestComments :+: sig) (PullRequestCommentsIOC m) where
  alg (R other) = PullRequestCommentsIOC (alg (handleCoercible other))
  alg (L effect) = do
    auth <- ask @Auth
    owner <- ask @(Name Owner)
    repo <- ask @(Name Repo)

    case effect of
      GetComment commentId k -> do
        result <- liftIO
          (github auth (pullRequestCommentR owner repo commentId))
        case result of
          Left err -> throwError @Error err
          Right comment -> k comment

      GetComments issueNumber fetchCount k -> do
        result <- liftIO
          (github auth (pullRequestCommentsR owner repo issueNumber fetchCount))
        case result of
          Left err -> throwError @Error err
          Right comments -> k comments

      CreateComment issueNumber commit path position body k -> do
        result <- liftIO (github auth
          (createPullCommentR owner repo issueNumber commit path position body))
        case result of
          Left err -> throwError @Error err
          Right comment -> k comment
