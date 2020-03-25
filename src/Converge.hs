{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Carrier.Trace.Printing (Trace, runTrace, trace)
import GHC.TypeLits (Symbol)
import qualified GitHub.Data as Data
import Servant ((:<|>) (..), (:>), Context ((:.)))
import qualified Servant
import qualified Servant.GitHub.Webhook as Servant

import Control.Carrier.Log.IO (Log, Severity (..), log, runLogIO)
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
    & runLogIO


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


type WebhookHandler event
   = Servant.RepoWebhookEvent
  -> ((), event)
  -> Servant.Handler ()


class ReflectWebhookEvent event where
  reflectWebhookEvent :: Servant.RepoWebhookEvent


instance ReflectWebhookEvent Data.PingEvent where
  reflectWebhookEvent = Data.WebhookPingEvent


instance ReflectWebhookEvent Data.PullRequestEvent where
  reflectWebhookEvent = Data.WebhookPullRequestEvent


webhookHandler
  :: forall event
   . ReflectWebhookEvent event
  => (event -> Servant.Handler ())
  -> WebhookHandler event
webhookHandler handler repoWebhookEvent (_, event) =
  if repoWebhookEvent == reflectWebhookEvent @event then
    handler event
  else
    pass


onHealthCheck :: Servant.Handler Text
onHealthCheck = pure "All good"


onPing :: WebhookHandler Data.PingEvent
onPing = webhookHandler \_event -> putTextLn "Pong!"


onPullRequest :: WebhookHandler Data.PullRequestEvent
onPullRequest = webhookHandler
  \(Data.PullRequestEvent action _number _pullRequest _repository _sender) -> do
    putTextLn ("pullRequestAction: " <> show action)

    case action of
      Data.PullRequestOpened ->
        pass

      Data.PullRequestClosed ->
        pass

      Data.PullRequestSynchronized ->
        pass

      Data.PullRequestReopened ->
        pass

      Data.PullRequestAssigned ->
        pass

      Data.PullRequestUnassigned ->
        pass

      Data.PullRequestLabeled ->
        pass

      Data.PullRequestUnlabeled ->
        pass

      Data.PullRequestReviewRequested ->
        pass

      Data.PullRequestReviewRequestRemoved ->
        pass

      Data.PullRequestEdited ->
        pass


server :: Servant.Server WebhookApi
server = onHealthCheck
    :<|> onPing
    :<|> onPullRequest


--------------------------------------------------------------------------------
-- Stupid hack to make servant-github-webhook work
-- https://github.com/tsani/servant-github-webhook/issues/13#issuecomment-408463124
--------------------------------------------------------------------------------


newtype GitHubKey = GitHubKey (forall result. Servant.GitHubKey result)


gitHubKey :: IO ByteString -> GitHubKey
gitHubKey k = GitHubKey (Servant.gitHubKey k)


instance Servant.HasContextEntry '[GitHubKey] (Servant.GitHubKey result) where
    getContextEntry (GitHubKey x :. _) = x
