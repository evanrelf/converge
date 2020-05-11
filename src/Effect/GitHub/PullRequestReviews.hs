{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Effect.GitHub.PullRequestReviews
  ( PullRequestReviews
  , review
  , reviews
  , reviewComments
  , pullRequestReviewsToIO
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
  Review :: G.IssueNumber -> G.Id G.Review -> PullRequestReviews m G.Review
  Reviews :: G.IssueNumber -> G.FetchCount -> PullRequestReviews m (Vector G.Review)
  ReviewComments :: G.IssueNumber -> G.Id G.Review -> PullRequestReviews m (Vector G.ReviewComment)

makeSem ''PullRequestReviews


up
  :: Members '[Error e, Embed IO] r
  => IO (Either e a)
  -> Sem r a
up = fromEither <=< embed


pullRequestReviewsToIO
  :: Member (Embed IO) r
  => G.Auth
  -> G.Name G.Owner
  -> G.Name G.Repo
  -> Sem (PullRequestReviews ': r) a
  -> Sem r (Either G.Error a)
pullRequestReviewsToIO auth owner repo
  = runError
  . reinterpret \case
    Review issueNumber reviewId -> up . github auth $
      Fns.pullRequestReviewR owner repo issueNumber reviewId

    Reviews issueNumber fetchCount -> up . github auth $
      Fns.pullRequestReviewsR owner repo issueNumber fetchCount

    ReviewComments issueNumber reviewId -> fmap Vector.fromList . up . github auth $
      Fns.pullRequestReviewCommentsR owner repo issueNumber reviewId
