{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Converge where


import Prelude hiding (id)

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, runM, sendM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import Data.Generics.Product (field)
import qualified Data.Map as Map
import Data.String.Interpolate (i)
import GHC.TypeLits (Symbol)
import qualified GitHub.Data as Data
import qualified GitHub.Data.Webhooks.Events as Events
import qualified GitHub.Data.Webhooks.Payload as Payload
import qualified Optics
import Optics ((%))
import Optics.Operators
import Servant ((:<|>) (..), (:>), Context ((:.)))
import qualified Servant
import qualified Servant.GitHub.Webhook as Servant

import Control.Carrier.Log.IO (Log, Verbosity (..), log, runLog)


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------


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


--------------------------------------------------------------------------------
-- API
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


--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------


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
    number
    _payload
    _repo
    _sender
    _installationId
  ) = do
  case action of
    Events.PullRequestAssignedAction -> do
      log Debug [i|Pull request ##{number}: assigned|]

    Events.PullRequestUnassignedAction -> do
      log Debug [i|Pull request ##{number}: unassigned|]

    Events.PullRequestReviewRequestedAction -> do
      log Debug [i|Pull request ##{number}: review requested|]

    Events.PullRequestReviewRequestRemovedAction -> do
      log Debug [i|Pull request ##{number}: review request removed|]

    Events.PullRequestLabeledAction -> do
      log Debug [i|Pull request ##{number}: labeled|]

    Events.PullRequestUnlabeledAction -> do
      log Debug [i|Pull request ##{number}: unlabeled|]

    Events.PullRequestOpenedAction -> do
      log Debug [i|Pull request ##{number}: opened|]

    Events.PullRequestEditedAction -> do
      log Debug [i|Pull request ##{number}: edited|]

    Events.PullRequestClosedAction -> do
      log Debug [i|Pull request ##{number}: closed|]

    Events.PullRequestReopenedAction -> do
      log Debug [i|Pull request ##{number}: reopened|]

    Events.PullRequestActionOther other -> do
      log Debug [i|Pull request ##{number}: unknown action '#{other}'|]


onIssueComment
  :: Has (Lift Servant.Handler) sig m
  => Has Log sig m
  => Events.IssueCommentEvent -> m ()
onIssueComment
  ( Events.IssueCommentEvent
    action
    issue
    _payload
    _repo
    _sender
  ) = do
  let number = Payload.whIssueNumber issue

  case action of
    Events.IssueCommentCreatedAction -> do
      log Debug [i|Issue ##{number}: comment created|]

    Events.IssueCommentEditedAction -> do
      log Debug [i|Issue ##{number}: comment edited|]

    Events.IssueCommentDeletedAction ->
      log Debug [i|Issue ##{number}: comment deleted|]

    Events.IssueCommentActionOther other -> do
      log Debug [i|Issue ##{number}: unknown comment action '#{other}'|]


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
    action
    _checkSuite
    _repository
    _organization
    _sender
    _installation
  ) = do
  case action of
    Events.CheckSuiteEventActionCompleted -> do
      log Debug "Check suite: completed"

    Events.CheckSuiteEventActionRequested -> do
      log Debug "Check suite: requested"

    Events.CheckSuiteEventActionRerequested -> do
      log Debug "Check suite: re-requested"

    Events.CheckSuiteEventActionOther other -> do
      log Debug [i|Check suite: Unknown action '#{other}'|]


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


--------------------------------------------------------------------------------


newtype Id a = Id Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)


data Event
  = PullRequestOpened (Id PullRequest)
  | PullRequestClosed (Id PullRequest)
  | IssueCommentCreated (Id Issue) IssueComment
  | IssueCommentEdited (Id Issue) Text
  | IssueCommentDeleted (Id Issue)
  deriving stock Show


data State = State
  { pullRequests :: Map (Id PullRequest) PullRequest
  , issueComments :: Map (Id Issue) IssueComment
  } deriving stock (Generic, Show)


data PullRequestState
  = Draft
  | Open
  | Merged
  | Closed
  deriving stock Show


data PullRequest = PullRequest
  { state :: PullRequestState
  } deriving stock (Generic, Show)

data Issue

data IssueComment = IssueComment
  { user :: Text
  , body :: Text
  } deriving stock (Generic, Show)


applyEvent :: State -> Event -> State
applyEvent state = \case
  PullRequestOpened id ->
    state & field @"pullRequests" % Optics.ix id % field @"state" .~ Open

  PullRequestClosed id ->
    state & field @"pullRequests" % Optics.ix id % field @"state" .~ Closed

  IssueCommentCreated id issueComment ->
    state & field @"issueComments" % Optics.ix id .~ issueComment

  IssueCommentEdited id body ->
    state & field @"issueComments" % Optics.ix id % field @"body" .~ body

  IssueCommentDeleted id ->
    state & field @"issueComments" %~ Map.delete id
