{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Effect.GitHub.PullRequestReviews
  ( PullRequestReviews
  , getReview
  , getReviews
  , getReviewComments
  , runPullRequestReviewsIO
  )
where

import Data.Vector as V (Vector, fromList)
import GitHub.Data as G (Auth, Error, FetchCount, Id, IssueNumber, Name, Owner, Repo, Review, ReviewComment)
import GitHub.Endpoints.PullRequests.Reviews
  ( pullRequestReviewCommentsR
  , pullRequestReviewR
  , pullRequestReviewsR
  )
import GitHub.Request (github)
import Polysemy
import Polysemy.Error as P (Error, fromEither, runError)


data PullRequestReviews m a where
  GetReview :: IssueNumber -> Id Review -> PullRequestReviews m Review
  GetReviews :: IssueNumber -> FetchCount -> PullRequestReviews m (Vector Review)
  GetReviewComments :: IssueNumber -> Id Review -> PullRequestReviews m (Vector ReviewComment)

makeSem ''PullRequestReviews


up
  :: MonadIO m
  => Members '[P.Error e, Embed m] r
  => IO (Either e a)
  -> Sem r a
up = fromEither <=< embed . liftIO


runPullRequestReviewsIO
  :: MonadIO m
  => Member (Embed m) r
  => Auth
  -> Name Owner
  -> Name Repo
  -> Sem (PullRequestReviews ': r) a
  -> Sem r (Either G.Error a)
runPullRequestReviewsIO auth owner repo
  = runError
  . reinterpret \case
    GetReview issueNumber reviewId -> up . github auth $
      pullRequestReviewR owner repo issueNumber reviewId

    GetReviews issueNumber fetchCount -> up . github auth $
      pullRequestReviewsR owner repo issueNumber fetchCount

    GetReviewComments issueNumber reviewId -> fmap V.fromList . up . github auth $
      pullRequestReviewCommentsR owner repo issueNumber reviewId
