{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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

{-# OPTIONS_GHC -Wno-name-shadowing  #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Converge where


import Prelude hiding (id)

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, runM, sendM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import Data.Generics.Product (field)
import Data.String.Interpolate (i)
import GHC.TypeLits (Symbol)
import qualified GitHub.Data as Data
import qualified GitHub.Data.Webhooks.Events as Events
import qualified GitHub.Data.Webhooks.Payload as Payload
import Optics ((%), ix, over, sans, set)
import Servant ((:<|>) (..), (:>), Context ((:.)))
import qualified Servant
import qualified Servant.GitHub.Webhook as ServantGW

import Control.Carrier.Log.IO (Log, Verbosity (..), log, runLog)


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------


type family Asum (xs :: [Type]) :: Type where
  Asum (x ': '[]) = x
  Asum (x ': xs) = x :<|> Asum xs


type (x :: Type) :# (summary :: Symbol) =
  Servant.Summary summary :> x


type Webhook (event :: Type) =
  "webhook"
    :> ServantGW.GitHubEvent '[ToWebhookEvent event]
    :> ServantGW.GitHubSignedReqBody '[Servant.JSON] event
    :> Servant.Post '[Servant.JSON] Servant.NoContent


type family ToWebhookEvent (event :: Type) :: ServantGW.RepoWebhookEvent where
  ToWebhookEvent Data.PingEvent = 'Data.WebhookPingEvent
  ToWebhookEvent Events.PullRequestEvent = 'Data.WebhookPullRequestEvent
  ToWebhookEvent Events.IssueCommentEvent = 'Data.WebhookIssueCommentEvent
  ToWebhookEvent Events.PushEvent = 'Data.WebhookPushEvent
  ToWebhookEvent Events.CheckSuiteEvent = 'Data.WebhookCheckSuiteEvent


runWebhookHandler
  :: forall event
   . ServantGW.Reflect (ToWebhookEvent event)
  => (event -> Servant.Handler ())
  -> ServantGW.RepoWebhookEvent
  -> ((), event)
  -> Servant.Handler Servant.NoContent
runWebhookHandler handler repoWebhookEvent ((), event) = do
  let proxy = Proxy @(ToWebhookEvent event)
  when (repoWebhookEvent == ServantGW.reflect proxy) (handler event)
  pure Servant.NoContent


sendH :: Has (Lift Servant.Handler) sig m => Servant.Handler a -> m a
sendH = sendM


--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------


type WebhookApi = Asum
  [ Webhook Data.PingEvent           :# "Ping from GitHub"
  , Webhook Events.PullRequestEvent  :# "Pull request event from GitHub"
  , Webhook Events.IssueCommentEvent :# "Issue comment event from GitHub"
  , Webhook Events.PushEvent         :# "Push event from GitHub"
  , Webhook Events.CheckSuiteEvent   :# "Check suite event from GitHub"
  , UnknownRequest                   :# "Unknown request to webhook path"
  , HealthCheck                      :# "Health check"
  ]


type UnknownRequest =
  "webhook"
    :> Servant.ReqBody '[Servant.JSON] Aeson.Value
    :> Servant.Post '[Servant.JSON] Servant.NoContent


type HealthCheck = "health" :> Servant.Get '[Servant.PlainText] Text


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
server = runWebhookHandler (runM . runLog verbosity . onPing)
    :<|> runWebhookHandler (runM . runLog verbosity . onPullRequest)
    :<|> runWebhookHandler (runM . runLog verbosity . onIssueComment)
    :<|> runWebhookHandler (runM . runLog verbosity . onPush)
    :<|> runWebhookHandler (runM . runLog verbosity . onCheckSuite)
    :<|> runM . runLog verbosity . onUnknown
    :<|> onHealthCheck
  where verbosity = Vomit


--------------------------------------------------------------------------------
-- Stupid hack to make servant-github-webhook work
-- https://github.com/tsani/servant-github-webhook/issues/13#issuecomment-408463124
--------------------------------------------------------------------------------


newtype GitHubKey = GitHubKey (forall result. ServantGW.GitHubKey result)


gitHubKey :: IO ByteString -> GitHubKey
gitHubKey k = GitHubKey (ServantGW.gitHubKey k)


instance Servant.HasContextEntry '[GitHubKey] (ServantGW.GitHubKey result) where
  getContextEntry (GitHubKey x :. _) = x


--------------------------------------------------------------------------------
-- Event sourcing stuff
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


applyEvent :: Event -> State -> State
applyEvent = \case
  PullRequestOpened id ->
    set (field @"pullRequests" % ix id % field @"state") Open

  PullRequestClosed id ->
    set (field @"pullRequests" % ix id % field @"state") Closed

  IssueCommentCreated id issueComment ->
    set (field @"issueComments" % ix id) issueComment

  IssueCommentEdited id body ->
    set (field @"issueComments" % ix id % field @"body") body

  IssueCommentDeleted id ->
    over (field @"issueComments") (sans id)
