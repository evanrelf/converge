{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Converge where


import Prelude hiding (trace)

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, runM, sendM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import GHC.TypeLits (Symbol)
import qualified GitHub.Data as Data
import qualified GitHub.Data.Webhooks.Events as Events
import Servant ((:<|>) (..), (:>), Context ((:.)))
import qualified Servant
import qualified Servant.GitHub.Webhook as Servant

import Control.Carrier.Log.IO (Log, Verbosity (..), log, runLog)


--------------------------------------------------------------------------------
-- Servant API
--------------------------------------------------------------------------------


type WebhookEndpoint (summary :: Symbol) (event :: Type) =
  "webhook"
    :> Servant.Summary summary
    :> Servant.GitHubEvent '[ToWebhookEvent event]
    :> Servant.GitHubSignedReqBody '[Servant.JSON] event
    :> Servant.Post '[Servant.JSON] Servant.NoContent


type WebhookApi =
  "health"
    :> Servant.Summary "Health check"
    :> Servant.Get '[Servant.PlainText] Text
    :<|>

  WebhookEndpoint
    "Ping from GitHub"
    Data.PingEvent
    :<|>

  WebhookEndpoint
    "Pull request event from GitHub"
    Events.PullRequestEvent
    :<|>

  WebhookEndpoint
    "Issue comment event from GitHub"
    Events.IssueCommentEvent
    :<|>

  WebhookEndpoint
    "Push event from GitHub"
    Events.PushEvent
    :<|>

  WebhookEndpoint
    "Check suite event from GitHub"
    Events.CheckSuiteEvent
    :<|>

  "webhook"
    :> Servant.Summary "Unknown request"
    :> Servant.ReqBody '[Servant.JSON] Aeson.Value
    :> Servant.Post '[Servant.JSON] Servant.NoContent


class ReflectWebhookEvent (event :: Type) where
  type ToWebhookEvent event :: Servant.RepoWebhookEvent
  reflectWebhookEvent :: Servant.RepoWebhookEvent


instance ReflectWebhookEvent Data.PingEvent where
  type ToWebhookEvent Data.PingEvent = 'Data.WebhookPingEvent
  reflectWebhookEvent = Data.WebhookPingEvent


instance ReflectWebhookEvent Events.PullRequestEvent where
  type ToWebhookEvent Events.PullRequestEvent = 'Data.WebhookPullRequestEvent
  reflectWebhookEvent = Data.WebhookPullRequestEvent


instance ReflectWebhookEvent Events.IssueCommentEvent where
  type ToWebhookEvent Events.IssueCommentEvent = 'Data.WebhookIssueCommentEvent
  reflectWebhookEvent = Data.WebhookIssueCommentEvent


instance ReflectWebhookEvent Events.PushEvent where
  type ToWebhookEvent Events.PushEvent = 'Data.WebhookPushEvent
  reflectWebhookEvent = Data.WebhookPushEvent


instance ReflectWebhookEvent Events.CheckSuiteEvent where
  type ToWebhookEvent Events.CheckSuiteEvent = 'Data.WebhookCheckSuiteEvent
  reflectWebhookEvent = Data.WebhookCheckSuiteEvent


runWebhookHandler
  :: forall event
   . ReflectWebhookEvent event
  => (event -> Servant.Handler ())
  -> Servant.RepoWebhookEvent
  -> ((), event)
  -> Servant.Handler Servant.NoContent
runWebhookHandler handler repoWebhookEvent ((), event) = do
  when (repoWebhookEvent == reflectWebhookEvent @event) (handler event)
  pure Servant.NoContent


sendH :: Has (Lift Servant.Handler) sig m => Servant.Handler a -> m a
sendH = sendM


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
  => Events.PullRequestEvent -> m ()
onPullRequest
  ( Events.PullRequestEvent
    action
    _number
    _payload
    _repo
    _sender
    _installationId
  ) = do
  case action of
    Events.PullRequestAssignedAction -> do
      log Debug "Pull request assigned"

    Events.PullRequestUnassignedAction -> do
      log Debug "Pull request unassigned"

    Events.PullRequestReviewRequestedAction -> do
      log Debug "Pull request review requested"

    Events.PullRequestReviewRequestRemovedAction -> do
      log Debug "Pull request review request removed"

    Events.PullRequestLabeledAction -> do
      log Debug "Pull request labeled"

    Events.PullRequestUnlabeledAction -> do
      log Debug "Pull request unlabeled"

    Events.PullRequestOpenedAction -> do
      log Debug "Pull request opened"

    Events.PullRequestEditedAction -> do
      log Debug "Pull request edited"

    Events.PullRequestClosedAction -> do
      log Debug "Pull request closed"

    Events.PullRequestReopenedAction -> do
      log Debug "Pull request reopened"

    Events.PullRequestActionOther other -> do
      log Debug ("Unknown pull request action '" <> other <> "'")


onIssueComment
  :: Has (Lift Servant.Handler) sig m
  => Has Log sig m
  => Events.IssueCommentEvent -> m ()
onIssueComment
  ( Events.IssueCommentEvent
    _action
    _issue
    _payload
    _repo
    _sender
  ) = do
  log Debug "Issue comment event"


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


onCheckSuite
  :: Has (Lift Servant.Handler) sig m
  => Has Log sig m
  => Events.CheckSuiteEvent
  -> m ()
onCheckSuite
  ( Events.CheckSuiteEvent
    _action
    _checkSuite
    _repository
    _organization
    _sender
    _installation
  ) = do
  log Debug "Check suite event"


onUnknown
  :: Has (Lift Servant.Handler) sig m
  => Has Log sig m
  => Aeson.Value
  -> m Servant.NoContent
onUnknown value = do
  log Vomit ("Unknown request: " <> toText (Aeson.encodeToLazyText value))
  pure Servant.NoContent


server :: Servant.Server WebhookApi
server = onHealthCheck
    :<|> runWebhookHandler (runM . runLog verbosity . onPing)
    :<|> runWebhookHandler (runM . runLog verbosity . onPullRequest)
    :<|> runWebhookHandler (runM . runLog verbosity . onIssueComment)
    :<|> runWebhookHandler (runM . runLog verbosity . onPush)
    :<|> runWebhookHandler (runM . runLog verbosity . onCheckSuite)
    :<|> runM . runLog verbosity . onUnknown
  where verbosity = Vomit


--------------------------------------------------------------------------------
-- Stupid hack to make servant-github-webhook work
-- https://github.com/tsani/servant-github-webhook/issues/13#issuecomment-408463124
--------------------------------------------------------------------------------


newtype GitHubKey = GitHubKey (forall result. Servant.GitHubKey result)


gitHubKey :: IO ByteString -> GitHubKey
gitHubKey k = GitHubKey (Servant.gitHubKey k)


instance Servant.HasContextEntry '[GitHubKey] (Servant.GitHubKey result) where
  getContextEntry (GitHubKey x :. _) = x
