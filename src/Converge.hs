{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Converge
  -- ( WebhookApi
  -- , server
  -- , gitHubKey
  -- )
where


import Control.Algebra (Has)
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
  => MonadIO m
  => m ()
program = do
  let issueNumber = Data.IssueNumber 1
  let body = "Hello world!"
  log Info "Creating comment"
  result <- createComment issueNumber body
  log Info "Printing result"
  print result
  log Info "Finished"


test :: ByteString -> IO (Either Data.Error ())
test token = do
  let auth = Data.OAuth token
  let owner = "evanrelf"
  let repo = "github-apps-test"
  program
    & runIssueCommentsIO auth owner repo
    & runLogIO


--------------------------------------------------------------------------------
-- Servant API
--------------------------------------------------------------------------------


type Webhook (summary :: Symbol) (webhook :: Servant.RepoWebhookEvent) event =
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

  Webhook "Ping from GitHub"
    'Data.WebhookPingEvent Data.PingEvent
    :<|>

  Webhook "Pull request event from GitHub"
    'Data.WebhookPullRequestEvent Data.PullRequestEvent


onPing
  :: Servant.RepoWebhookEvent
  -> ((), Data.PingEvent)
  -> Servant.Handler ()
onPing Data.WebhookPingEvent (_, event) =
  putTextLn ("PingEvent: " <> show event)
onPing _ _ = pass


onPullRequest
  :: Servant.RepoWebhookEvent
  -> ((), Data.PullRequestEvent)
  -> Servant.Handler ()
onPullRequest Data.WebhookPullRequestEvent (_, event) =
  putTextLn ("PullRequstEvent: " <> show event)
onPullRequest _ _ = pass


onHealthCheck :: Servant.Handler Text
onHealthCheck = pure "All good"


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
