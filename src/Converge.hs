{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Converge
  -- ( WebhookApi
  -- , server
  -- , gitHubKey
  -- )
where


import Prelude hiding (trace)

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, runM, sendM)
import Control.Carrier.Trace.Printing (Trace, runTrace, trace)
import GHC.TypeLits (Symbol)
import qualified GitHub.Data as Data
import qualified GitHub.Data.Webhooks.Events as Events
import Servant ((:<|>) (..), (:>), Context ((:.)))
import qualified Servant
import qualified Servant.GitHub.Webhook as Servant

import Control.Carrier.Log.IO (Log, Severity (..), log, runLog)
import GitHub.Carrier.Issue.Comments.IO
  ( IssueComments
  , createComment
  , runIssueCommentsIO
  )


--------------------------------------------------------------------------------
-- Scratchpad
--------------------------------------------------------------------------------


program
  :: Has IssueComments sig m
  => Has Log sig m
  => Has Trace sig m
  => m ()
program = do
  let issueNumber = Data.IssueNumber 1
  let body = "Hello world!"
  log Info "Creating comment"
  result <- createComment issueNumber body
  log Info "Tracing result"
  trace (show result)
  log Info "Finished"


test :: ByteString -> IO (Either Data.Error ())
test token = do
  let auth = Data.OAuth token
  let owner = "evanrelf"
  let repo = "github-apps-test"
  program
    & runIssueCommentsIO auth owner repo
    & runTrace
    & runLog


--------------------------------------------------------------------------------
-- Servant API
--------------------------------------------------------------------------------


type WebhookEndpoint
  (summary :: Symbol)
  (webhook :: Servant.RepoWebhookEvent)
  (event :: Type) =
  "webhook"
    :> Servant.Summary summary
    :> Servant.GitHubEvent '[webhook]
    :> Servant.GitHubSignedReqBody '[Servant.JSON] event
    :> Servant.Post '[Servant.JSON] ()


type WebhookApi =
  "health"
    :> Servant.Summary "Health check"
    :> Servant.Get '[Servant.PlainText] Text
    :<|>

  WebhookEndpoint "Ping from GitHub"
    'Data.WebhookPingEvent Data.PingEvent
    :<|>

  WebhookEndpoint "Pull request event from GitHub"
    'Data.WebhookPullRequestEvent Data.PullRequestEvent
    :<|>

  WebhookEndpoint "Push event from GitHub"
    'Data.WebhookPushEvent Events.PushEvent


class ReflectWebhookEvent event where
  reflectWebhookEvent :: Servant.RepoWebhookEvent


instance ReflectWebhookEvent Data.PingEvent where
  reflectWebhookEvent = Data.WebhookPingEvent


instance ReflectWebhookEvent Data.PullRequestEvent where
  reflectWebhookEvent = Data.WebhookPullRequestEvent


instance ReflectWebhookEvent Events.PushEvent where
  reflectWebhookEvent = Data.WebhookPushEvent


wrapWebhookHandler
  :: forall sig m event
   . Has (Lift Servant.Handler) sig m
  => ReflectWebhookEvent event
  => (event -> m ())
  -> Servant.RepoWebhookEvent
  -> ((), event)
  -> m ()
wrapWebhookHandler handler repoWebhookEvent ((), event) =
  if repoWebhookEvent == reflectWebhookEvent @event then
    handler event
  else
    pass


sendH :: Has (Lift Servant.Handler) sig m => Servant.Handler a -> m a
sendH = sendM


runWebhookHandler
  :: ReflectWebhookEvent event
  => (event -> _m ())
  -> Servant.RepoWebhookEvent
  -> ((), event)
  -> Servant.Handler ()
runWebhookHandler handler x y = runM (wrapWebhookHandler handler x y)


onHealthCheck :: Servant.Handler Text
onHealthCheck = pure "All good"


onPing
  :: Has (Lift Servant.Handler) sig m
  => Has Log sig m
  => Data.PingEvent -> m ()
onPing _event = log Debug "Pong!"


onPullRequest
  :: Has (Lift Servant.Handler) sig m
  => Has Log sig m
  => Data.PullRequestEvent -> m ()
onPullRequest
  ( Data.PullRequestEvent
    action
    _number
    _pullRequest
    _repository
    _sender
  ) = do
  case action of
    Data.PullRequestOpened -> do
      log Debug "Pull request opened"
      pass

    Data.PullRequestClosed -> do
      log Debug "Pull request closed"
      pass

    Data.PullRequestSynchronized -> do
      log Debug "Pull request synchronized"
      pass

    Data.PullRequestReopened -> do
      log Debug "Pull request reopened"
      pass

    Data.PullRequestAssigned -> do
      log Debug "Pull request assigned"
      pass

    Data.PullRequestUnassigned -> do
      log Debug "Pull request unassigned"
      pass

    Data.PullRequestLabeled -> do
      log Debug "Pull request labeled"
      pass

    Data.PullRequestUnlabeled -> do
      log Debug "Pull request unlabeled"
      pass

    Data.PullRequestReviewRequested -> do
      log Debug "Pull request review requested"
      pass

    Data.PullRequestReviewRequestRemoved -> do
      log Debug "Pull request review request removed"
      pass

    Data.PullRequestEdited -> do
      log Debug "Pull request edited"
      pass


onPush
  :: Has (Lift Servant.Handler) sig m
  => Has Log sig m
  => Events.PushEvent
  -> m ()
onPush
  ( Events.PushEvent
    _ref
    _headSha
    _beforeSha
    _created
    _deleted
    _forced
    _baseRef
    _compareUrl
    _commits
    _headCommit
    _repository
    _organization
    _sender
  ) = do
  log Debug "Push event"


server :: Servant.Server WebhookApi
server = onHealthCheck
    :<|> runWebhookHandler (runLog . onPing)
    :<|> runWebhookHandler (runLog . onPullRequest)
    :<|> runWebhookHandler (runLog . onPush)


--------------------------------------------------------------------------------
-- Stupid hack to make servant-github-webhook work
-- https://github.com/tsani/servant-github-webhook/issues/13#issuecomment-408463124
--------------------------------------------------------------------------------


newtype GitHubKey = GitHubKey (forall result. Servant.GitHubKey result)


gitHubKey :: IO ByteString -> GitHubKey
gitHubKey k = GitHubKey (Servant.gitHubKey k)


instance Servant.HasContextEntry '[GitHubKey] (Servant.GitHubKey result) where
  getContextEntry (GitHubKey x :. _) = x
