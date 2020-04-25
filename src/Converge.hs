{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Converge where

import Prelude hiding (id)

import Control.Concurrent.Classy (MonadConc)
import qualified Control.Concurrent.Classy as Concurrent
import qualified Control.Concurrent.Classy.Async as Async
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import Data.Generics.Product (field)
import Data.String.Interpolate (i)
import GHC.TypeLits (Symbol)
import Generic.Data (Generically (..))
import qualified GitHub.Data as Data
import qualified GitHub.Data.Webhooks.Events as Events
import qualified GitHub.Data.Webhooks.Payload as Payload
import qualified Network.Wai.Handler.Warp as Warp
import Optics ((%), at, ix, over, sans, set)
import Polysemy (Member, Sem, runM)
import Servant ((:<|>) (..), (:>), Context ((:.)))
import qualified Servant
import qualified Servant.GitHub.Webhook as ServantGW

import Effect.Log (Log, Verbosity (..), log, runLogIO)


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------


main :: IO ()
main = do
  let launch = Async.runConcurrently . asum . fmap Async.Concurrently

  launch [api, eventLog]


api :: (MonadIO m, MonadConc m) => m ()
api = do
  let host = "localhost" :: Text
  let port = 7777
  let secret = "super-secret-code"

  putTextLn [i|Running at http://#{host}:#{port}|]
  liftIO $ Warp.run port
    (Servant.serveWithContext
      (Proxy @Api)
      (gitHubKey (pure secret) :. Servant.EmptyContext)
      server)


eventLog :: (MonadIO m, MonadConc m) => m ()
eventLog = forever do
  putTextLn "Fake event log"
  Concurrent.threadDelay (5 * 1_000_000)


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


webhookHandler
  :: forall event
   . ServantGW.Reflect (ToWebhookEvent event)
  => (event -> Servant.Handler ())
  -> ServantGW.RepoWebhookEvent
  -> ((), event)
  -> Servant.Handler Servant.NoContent
webhookHandler handler repoWebhookEvent ((), event) = do
  let proxy = Proxy @(ToWebhookEvent event)
  when (repoWebhookEvent == ServantGW.reflect proxy) (handler event)
  pure Servant.NoContent


--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------


type Api = WebhookApi :<|> DebugApi


type WebhookApi = Asum
  [ Webhook Data.PingEvent           :# "Ping from GitHub"
  , Webhook Events.PullRequestEvent  :# "Pull request event from GitHub"
  , Webhook Events.IssueCommentEvent :# "Issue comment event from GitHub"
  , Webhook Events.PushEvent         :# "Push event from GitHub"
  , Webhook Events.CheckSuiteEvent   :# "Check suite event from GitHub"
  , UnknownRequest                   :# "Unknown request to webhook path"
  ]


type DebugApi = HealthCheck


type UnknownRequest =
  "webhook"
    :> Servant.ReqBody '[Servant.JSON] Aeson.Value
    :> Servant.Post '[Servant.JSON] Servant.NoContent


type HealthCheck =
  "health"
    :> Servant.Summary "Health check"
    :> Servant.Get '[Servant.PlainText] Text


--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------


onHealthCheck :: Servant.Handler Text
onHealthCheck = pure "All good"


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
      let pullRequest = PullRequest
            { state = Open
            }
      pushEvent (PullRequestOpened (Id evPullReqNumber) pullRequest)

    Events.PullRequestEditedAction -> do
      log Debug [i|Pull request ##{evPullReqNumber}: edited|]

    Events.PullRequestClosedAction -> do
      log Debug [i|Pull request ##{evPullReqNumber}: closed|]
      pushEvent (PullRequestClosed (Id evPullReqNumber))

    Events.PullRequestReopenedAction -> do
      log Debug [i|Pull request ##{evPullReqNumber}: reopened|]
      pushEvent (PullRequestReopened (Id evPullReqNumber))

    Events.PullRequestActionOther other -> do
      log Debug [i|Pull request ##{evPullReqNumber}: unknown action '#{other}'|]


onIssueComment :: Member Log r => Events.IssueCommentEvent -> Sem r ()
onIssueComment Events.IssueCommentEvent{..} = do
  let Payload.HookIssue{whIssueNumber} = evIssueCommentIssue
  let Payload.HookIssueComment{whIssueCommentBody, whIssueCommentUser} = evIssueCommentPayload
  let Payload.HookUser{whUserLogin} = whIssueCommentUser

  case evIssueCommentAction of
    Events.IssueCommentCreatedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: comment created|]
      let issueComment = IssueComment
            { body = whIssueCommentBody
            , user = whUserLogin
            }
      pushEvent (IssueCommentCreated (Id whIssueNumber) issueComment)

    Events.IssueCommentEditedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: comment edited|]
      pushEvent (IssueCommentEdited (Id whIssueNumber) whIssueCommentBody)

    Events.IssueCommentDeletedAction -> do
      log Debug [i|Issue ##{whIssueNumber}: comment deleted|]
      pushEvent (IssueCommentDeleted (Id whIssueNumber))

    Events.IssueCommentActionOther other -> do
      log Debug [i|Issue ##{whIssueNumber}: unknown comment action '#{other}'|]


onPush :: Member Log r => Events.PushEvent -> Sem r ()
onPush Events.PushEvent{..} = do
  log Debug "Push event"


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


server :: Servant.Server Api
server = webhookServer :<|> debugServer


webhookServer :: Servant.Server WebhookApi
webhookServer =
       webhookHandler (runM . runLogIO verbosity . onPing)
  :<|> webhookHandler (runM . runLogIO verbosity . onPullRequest)
  :<|> webhookHandler (runM . runLogIO verbosity . onIssueComment)
  :<|> webhookHandler (runM . runLogIO verbosity . onPush)
  :<|> webhookHandler (runM . runLogIO verbosity . onCheckSuite)
  :<|> runM . runLogIO verbosity . onUnknown
  where verbosity = Vomit


debugServer :: Servant.Server DebugApi
debugServer = onHealthCheck


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


data Event
  = PullRequestOpened (Id PullRequest) PullRequest
  | PullRequestClosed (Id PullRequest)
  | PullRequestReopened (Id PullRequest)
  | IssueCommentCreated (Id Issue) IssueComment
  | IssueCommentEdited (Id Issue) Text
  | IssueCommentDeleted (Id Issue)
  deriving stock Show


data State = State
  { pullRequests :: Map (Id PullRequest) PullRequest
  , issueComments :: Map (Id Issue) IssueComment
  } deriving stock (Generic, Show)
    deriving Semigroup via Generically State
    deriving Monoid via Generically State


pushEvent :: Event -> m ()
pushEvent = undefined


applyEvent :: State -> Event -> State
applyEvent state = (state &) . \case
  PullRequestOpened id pullRequest ->
    set (field @"pullRequests" % at id) (Just pullRequest)

  PullRequestClosed id ->
    set (field @"pullRequests" % ix id % field @"state") Closed

  PullRequestReopened id ->
    set (field @"pullRequests" % ix id % field @"state") Open

  IssueCommentCreated id issueComment ->
    set (field @"issueComments" % at id) (Just issueComment)

  IssueCommentEdited id body ->
    set (field @"issueComments" % ix id % field @"body") body

  IssueCommentDeleted id ->
    over (field @"issueComments") (sans id)
