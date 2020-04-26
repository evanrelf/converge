{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Effect.GitHub.PullRequests
  ( PullRequests
  , pullRequest
  , pullRequests
  , createPullRequest
  , updatePullRequest
  , pullRequestCommits
  , pullRequestFiles
  , isPullRequestMerged
  , mergePullRequest
  , runPullRequestsIO
  )
where

import Data.Vector (Vector)
import qualified GitHub.Data as G
import qualified GitHub.Endpoints.PullRequests as Fns
import GitHub.Request (github)
import Polysemy
import Polysemy.Error (Error, fromEither, runError)


data PullRequests m a where
  PullRequest :: G.IssueNumber -> PullRequests m G.PullRequest
  PullRequests :: G.PullRequestMod -> G.FetchCount -> PullRequests m (Vector G.SimplePullRequest)
  CreatePullRequest :: G.CreatePullRequest -> PullRequests m G.PullRequest
  UpdatePullRequest :: G.IssueNumber -> G.EditPullRequest -> PullRequests m G.PullRequest
  PullRequestCommits :: G.IssueNumber -> G.FetchCount -> PullRequests m (Vector G.Commit)
  PullRequestFiles :: G.IssueNumber -> G.FetchCount -> PullRequests m (Vector G.File)
  IsPullRequestMerged :: G.IssueNumber -> PullRequests m Bool
  MergePullRequest :: G.IssueNumber -> Maybe Text -> PullRequests m G.MergeResult

makeSem ''PullRequests


up
  :: MonadIO m
  => Members '[Error e, Embed m] r
  => IO (Either e a)
  -> Sem r a
up = fromEither <=< embed . liftIO


runPullRequestsIO
  :: MonadIO m
  => Member (Embed m) r
  => G.Auth
  -> G.Name G.Owner
  -> G.Name G.Repo
  -> Sem (PullRequests ': r) a
  -> Sem r (Either G.Error a)
runPullRequestsIO auth owner repo
  = runError
  . reinterpret \case
    PullRequest issueNumber -> up . github auth $
      Fns.pullRequestR owner repo issueNumber

    PullRequests pullRequestMod fetchCount -> up . github auth $
      Fns.pullRequestsForR owner repo pullRequestMod fetchCount

    CreatePullRequest createPullRequest' -> up . github auth $
      Fns.createPullRequestR owner repo createPullRequest'

    UpdatePullRequest issueNumber editPullRequest -> up . github auth $
      Fns.updatePullRequestR owner repo issueNumber editPullRequest

    PullRequestCommits issueNumber fetchCount -> up . github auth $
      Fns.pullRequestCommitsR owner repo issueNumber fetchCount

    PullRequestFiles issueNumber fetchCount -> up . github auth $
      Fns.pullRequestFilesR owner repo issueNumber fetchCount

    IsPullRequestMerged issueNumber -> up . github auth $
      Fns.isPullRequestMergedR owner repo issueNumber

    MergePullRequest issueNumber commitMessage -> up . github auth $
      Fns.mergePullRequestR owner repo issueNumber commitMessage
