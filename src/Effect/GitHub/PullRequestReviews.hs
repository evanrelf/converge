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

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified GitHub.Data as G
import qualified GitHub.Endpoints.PullRequests.Reviews as Fns
import GitHub.Request (github)
import Polysemy
import Polysemy.Error (Error, fromEither, runError)


data PullRequestReviews m a where
  GetReview :: G.IssueNumber -> G.Id G.Review -> PullRequestReviews m G.Review
  GetReviews :: G.IssueNumber -> G.FetchCount -> PullRequestReviews m (Vector G.Review)
  GetReviewComments :: G.IssueNumber -> G.Id G.Review -> PullRequestReviews m (Vector G.ReviewComment)

makeSem ''PullRequestReviews


up
  :: MonadIO m
  => Members '[Error e, Embed m] r
  => IO (Either e a)
  -> Sem r a
up = fromEither <=< embed . liftIO


runPullRequestReviewsIO
  :: MonadIO m
  => Member (Embed m) r
  => G.Auth
  -> G.Name G.Owner
  -> G.Name G.Repo
  -> Sem (PullRequestReviews ': r) a
  -> Sem r (Either G.Error a)
runPullRequestReviewsIO auth owner repo
  = runError
  . reinterpret \case
    GetReview issueNumber reviewId -> up . github auth $
      Fns.pullRequestReviewR owner repo issueNumber reviewId

    GetReviews issueNumber fetchCount -> up . github auth $
      Fns.pullRequestReviewsR owner repo issueNumber fetchCount

    GetReviewComments issueNumber reviewId -> fmap Vector.fromList . up . github auth $
      Fns.pullRequestReviewCommentsR owner repo issueNumber reviewId
