{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Control.Carrier.GitHub.PullRequestComments.IO
  ( PullRequestCommentsIOC (..)
  , runPullRequestCommentsIO
  -- Re-exports
  , module Control.Effect.GitHub.PullRequestComments
  )
where


import Control.Algebra ((:+:) (..), alg, handleCoercible)
import Control.Carrier.Lift (Lift, runM, sendM)
import Control.Carrier.Reader (Reader, ask, runReader)
import Control.Carrier.Throw.Either (Throw, runThrow, throwError)
import GitHub.Data (Auth, Error, Name, Owner, Repo)
import GitHub.Endpoints.PullRequests.Comments
  ( pullRequestCommentR
  , pullRequestCommentsR
  , createPullCommentR
  )
import GitHub.Request (github)

import Control.Effect.GitHub.PullRequestComments


runPullRequestCommentsIO
  :: Auth
  -> Name Owner
  -> Name Repo
  -> _m a
  -> IO (Either Error a)
runPullRequestCommentsIO auth owner repo
  = runM
  . runThrow
  . runReader repo
  . runReader owner
  . runReader auth
  . runPullRequestCommentsIOC


newtype PullRequestCommentsIOC m a
  = PullRequestCommentsIOC { runPullRequestCommentsIOC :: m a }
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
  => Algebra (PullRequestComments :+: sig) (PullRequestCommentsIOC m) where
  alg (R other) = PullRequestCommentsIOC (alg (handleCoercible other))
  alg (L effect) = do
    auth <- ask @Auth
    owner <- ask @(Name Owner)
    repo <- ask @(Name Repo)

    case effect of
      GetComment commentId k -> do
        result <- sendM @IO
          (github auth (pullRequestCommentR owner repo commentId))
        case result of
          Left err -> throwError @Error err
          Right comment -> k comment

      GetComments issueNumber fetchCount k -> do
        result <- sendM @IO
          (github auth (pullRequestCommentsR owner repo issueNumber fetchCount))
        case result of
          Left err -> throwError @Error err
          Right comments -> k comments

      CreateComment issueNumber commit path position body k -> do
        result <- sendM @IO
          (github auth
            (createPullCommentR
              owner
              repo
              issueNumber
              commit
              path
              position
              body))
        case result of
          Left err -> throwError @Error err
          Right comment -> k comment
