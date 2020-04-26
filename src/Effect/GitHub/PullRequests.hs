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
import GitHub.Data as G (Auth, Error, FetchCount, IssueNumber, Name, Owner, Repo, MergeResult, File, Commit, EditPullRequest, SimplePullRequest, PullRequestMod, PullRequest, CreatePullRequest)
import qualified GitHub.Endpoints.PullRequests as Fns
import GitHub.Request (github)
import Polysemy
import Polysemy.Error as P (Error, fromEither, runError)


data PullRequests m a where
  PullRequest :: IssueNumber -> PullRequests m PullRequest
  PullRequests :: PullRequestMod -> FetchCount -> PullRequests m (Vector SimplePullRequest)
  CreatePullRequest :: CreatePullRequest -> PullRequests m PullRequest
  UpdatePullRequest :: IssueNumber -> EditPullRequest -> PullRequests m PullRequest
  PullRequestCommits :: IssueNumber -> FetchCount -> PullRequests m (Vector Commit)
  PullRequestFiles :: IssueNumber -> FetchCount -> PullRequests m (Vector File)
  IsPullRequestMerged :: IssueNumber -> PullRequests m Bool
  MergePullRequest :: IssueNumber -> Maybe Text -> PullRequests m MergeResult

makeSem ''PullRequests


up
  :: MonadIO m
  => Members '[P.Error e, Embed m] r
  => IO (Either e a)
  -> Sem r a
up = fromEither <=< embed . liftIO


runPullRequestsIO
  :: MonadIO m
  => Member (Embed m) r
  => Auth
  -> Name Owner
  -> Name Repo
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
