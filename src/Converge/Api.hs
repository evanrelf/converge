{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Converge.Api (run) where

import Converge.State (World)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import Data.String.Interpolate (i)
import Effect.Log (Log, Verbosity (..), log)
import GHC.TypeLits (Symbol)
import qualified GitHub.Data as Data
import qualified GitHub.Data.Webhooks.Events as Events
import qualified GitHub.Data.Webhooks.Payload as Payload
import qualified Network.Wai.Handler.Warp as Warp
import Polysemy (Member, Members, Sem)
import Polysemy.AtomicState (AtomicState, atomicGet)
import Servant ((:<|>) (..), (:>), Context ((:.)))
import qualified Servant
import qualified Servant.GitHub.Webhook as ServantGW


run
  :: MonadIO m
  => Members '[AtomicState World, Log] r
  => Int
  -> ByteString
  -> (forall a. Sem r a -> Servant.Handler a)
  -> m ()
run port secret interpreter = liftIO do
  putTextLn [i|Running at http://localhost:#{port}|]
  Warp.run port
    (Servant.serveWithContext
      (Proxy @Api)
      (gitHubKey (pure secret) :. Servant.EmptyContext)
      (Servant.hoistServerWithContext
        (Proxy @Api)
        (Proxy @'[GitHubKey])
        interpreter
        server))


--------------------------------------------------------------------------------
-- APIs
--------------------------------------------------------------------------------


type Api = WebhookApi :<|> DebugApi


type WebhookApi = Asum
  [ Webhook Data.PingEvent                :# "Ping"
  , Webhook Events.PullRequestEvent       :# "Pull request event"
  , Webhook Events.PullRequestReviewEvent :# "Pull request review event"
  , Webhook Events.IssuesEvent            :# "Issue event"
  , Webhook Events.IssueCommentEvent      :# "Issue comment event"
  , Webhook Events.PushEvent              :# "Push event"
  , Webhook Events.StatusEvent            :# "Status event"
  , Webhook Events.CheckSuiteEvent        :# "Check suite event"
  , UnknownRequest                        :# "Unknown request"
  ]


type DebugApi =
  "debug" :> (HealthCheck :<|> DumpState)


type UnknownRequest =
  "webhook"
    :> Servant.ReqBody '[Servant.JSON] Aeson.Value
    :> Servant.Post '[Servant.JSON] Servant.NoContent


type HealthCheck =
  "health-check"
    :> Servant.Summary "Health check"
    :> Servant.Get '[Servant.PlainText] Text


type DumpState =
  "dump-state"
    :> Servant.Summary "Dump state"
    :> Servant.Get '[Servant.JSON] Aeson.Value


--------------------------------------------------------------------------------
-- Servers
--------------------------------------------------------------------------------


server :: Members '[AtomicState World, Log] r => Servant.ServerT Api (Sem r)
server = webhookServer :<|> debugServer


webhookServer :: Member Log r => Servant.ServerT WebhookApi (Sem r)
webhookServer
     = webhookHandler onPing
  :<|> webhookHandler onPullRequest
  :<|> webhookHandler onPullRequestReview
  :<|> webhookHandler onIssue
  :<|> webhookHandler onIssueComment
  :<|> webhookHandler onPush
  :<|> webhookHandler onStatus
  :<|> webhookHandler onCheckSuite
  :<|> onUnknown


debugServer
  :: Members '[AtomicState World, Log] r
  => Servant.ServerT DebugApi (Sem r)
debugServer = onHealthCheck :<|> onDumpState


--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------


onPing :: Member Log r => Data.PingEvent -> Sem r ()
onPing _event = log Debug "Pong!"


onPullRequest :: Member Log r => Events.PullRequestEvent -> Sem r ()
onPullRequest Events.PullRequestEvent{..} = do
  case evPullReqAction of
    Events.PullRequestAssignedAction -> do
      log Debug [i|Pull request ##{evPullReqNumber}: assigned|]

    Events.PullRequestUnassignedAction -> do
      log Debug [i|Pull request ##{evPullReqNumber}: unassigned|]

    Events.PullRequestReviewRequestedAction -> do
      log Debug [i|Pull request ##{evPullReqNumber}: review requested|]

    Events.PullRequestReviewRequestRemovedAction -> do
      log Debug [i|Pull request ##{evPullReqNumber}: review request removed|]

    Events.PullRequestLabeledAction -> do
      log Debug [i|Pull request ##{evPullReqNumber}: labeled|]

    Events.PullRequestUnlabeledAction -> do
      log Debug [i|Pull request ##{evPullReqNumber}: unlabeled|]

    Events.PullRequestOpenedAction -> do
      log Debug [i|Pull request ##{evPullReqNumber}: opened|]

    Events.PullRequestEditedAction -> do
      log Debug [i|Pull request ##{evPullReqNumber}: edited|]

    Events.PullRequestClosedAction -> do
      log Debug [i|Pull request ##{evPullReqNumber}: closed|]

    Events.PullRequestReopenedAction -> do
      log Debug [i|Pull request ##{evPullReqNumber}: reopened|]

    Events.PullRequestActionOther other -> do
      log Debug [i|Pull request ##{evPullReqNumber}: unknown action '#{other}'|]


onPullRequestReview :: Member Log r => Events.PullRequestReviewEvent -> Sem r ()
onPullRequestReview Events.PullRequestReviewEvent{..} = do
  let Payload.HookPullRequest{whPullReqNumber} = evPullReqReviewTarget

  case evPullReqReviewAction of
    Events.PullRequestReviewSubmittedAction -> do
      log Debug [i|Pull request ##{whPullReqNumber}: review submitted|]

    Events.PullRequestReviewEditedAction -> do
      log Debug [i|Pull request ##{whPullReqNumber}: review edited|]

    Events.PullRequestReviewDismissedAction -> do
      log Debug [i|Pull request ##{whPullReqNumber}: review dismissed|]

    Events.PullRequestReviewActionOther other -> do
      log Debug [i|Pull request ##{whPullReqNumber}: unknown review action '#{other}'|]


onIssue :: Member Log r => Events.IssuesEvent -> Sem r ()
onIssue Events.IssuesEvent{..} = do
  let Payload.HookIssue{whIssueNumber} = evIssuesEventIssue

  case evIssuesEventAction of
    Events.IssuesAssignedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: assigned|]

    Events.IssuesUnassignedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: unassigned|]

    Events.IssuesLabeledAction -> do
      log Debug [i|Issue ##{whIssueNumber}: labeled|]

    Events.IssuesUnlabeledAction -> do
      log Debug [i|Issue ##{whIssueNumber}: unlabeled|]

    Events.IssuesOpenedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: opened|]

    Events.IssuesEditedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: edited|]

    Events.IssuesMilestonedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: milestoned|]

    Events.IssuesDemilestonedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: demilestoned|]

    Events.IssuesClosedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: closed|]

    Events.IssuesReopenedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: reopened|]

    Events.IssuesActionOther other -> do
      log Debug [i|Issue ##{whIssueNumber}: unknown action '#{other}'|]


onIssueComment :: Member Log r => Events.IssueCommentEvent -> Sem r ()
onIssueComment Events.IssueCommentEvent{..} = do
  let Payload.HookIssue{whIssueNumber} = evIssueCommentIssue

  case evIssueCommentAction of
    Events.IssueCommentCreatedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: comment created|]

    Events.IssueCommentEditedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: comment edited|]

    Events.IssueCommentDeletedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: comment deleted|]

    Events.IssueCommentActionOther other -> do
      log Debug [i|Issue ##{whIssueNumber}: unknown comment action '#{other}'|]


onPush :: Member Log r => Events.PushEvent -> Sem r ()
onPush Events.PushEvent{..} = do
  log Debug "Push event"


onStatus :: Member Log r => Events.StatusEvent -> Sem r ()
onStatus Events.StatusEvent{..} = do
  log Debug "Status event"


onCheckSuite :: Member Log r => Events.CheckSuiteEvent -> Sem r ()
onCheckSuite Events.CheckSuiteEvent{..} = do
  case evCheckSuiteAction of
    Events.CheckSuiteEventActionCompleted -> do
      log Debug "Check suite: completed"

    Events.CheckSuiteEventActionRequested -> do
      log Debug "Check suite: requested"

    Events.CheckSuiteEventActionRerequested -> do
      log Debug "Check suite: re-requested"

    Events.CheckSuiteEventActionOther other -> do
      log Debug [i|Check suite: Unknown action '#{other}'|]


onUnknown :: Member Log r => Aeson.Value -> Sem r Servant.NoContent
onUnknown value = do
  log Vomit ("Unknown request: " <> toText (Aeson.encodeToLazyText value))
  pure Servant.NoContent


onHealthCheck :: Sem r Text
onHealthCheck = pure "All good"


onDumpState :: Members '[AtomicState World, Log] r => Sem r Aeson.Value
onDumpState = do
  log Debug "Dumping state"
  Aeson.toJSON <$> atomicGet


--------------------------------------------------------------------------------
-- Helper Functions
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
  ToWebhookEvent Events.PullRequestReviewEvent = 'Data.WebhookPullRequestReviewEvent
  ToWebhookEvent Events.IssuesEvent = 'Data.WebhookIssuesEvent
  ToWebhookEvent Events.IssueCommentEvent = 'Data.WebhookIssueCommentEvent
  ToWebhookEvent Events.PushEvent = 'Data.WebhookPushEvent
  ToWebhookEvent Events.StatusEvent = 'Data.WebhookStatusEvent
  ToWebhookEvent Events.CheckSuiteEvent = 'Data.WebhookCheckSuiteEvent


webhookHandler
  :: forall event m
   . Monad m
  => ServantGW.Reflect (ToWebhookEvent event)
  => (event -> m ())
  -> ServantGW.RepoWebhookEvent
  -> ((), event)
  -> m Servant.NoContent
webhookHandler handler repoWebhookEvent ((), event) = do
  let proxy = Proxy @(ToWebhookEvent event)
  when (repoWebhookEvent == ServantGW.reflect proxy) (handler event)
  pure Servant.NoContent


--------------------------------------------------------------------------------
-- Stupid hack to make servant-github-webhook work
-- https://github.com/tsani/servant-github-webhook/issues/13#issuecomment-408463124
--------------------------------------------------------------------------------


newtype GitHubKey = GitHubKey (forall result. ServantGW.GitHubKey result)


gitHubKey :: IO ByteString -> GitHubKey
gitHubKey k = GitHubKey (ServantGW.gitHubKey k)


instance Servant.HasContextEntry '[GitHubKey] (ServantGW.GitHubKey result) where
  getContextEntry (GitHubKey x :. _) = x
