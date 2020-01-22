{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Converge
  ( WebhookApi
  , server
  , gitHubKey
  )
where

import qualified GitHub.Data as GitHub
import Servant ((:<|>) (..), (:>), Context ((:.)))
import qualified Servant
import qualified Servant.GitHub.Webhook as Servant


type WebhookApi =
  "webhook"
    :> Servant.Summary "Ping from GitHub"
    :> Servant.GitHubEvent '[ 'GitHub.WebhookPingEvent ]
    :> Servant.GitHubSignedReqBody '[Servant.JSON] GitHub.PingEvent
    :> Servant.Post '[Servant.JSON] ()
  :<|>
  "webhook"
    :> Servant.Summary "Pull request event from GitHub"
    :> Servant.GitHubEvent '[ 'GitHub.WebhookPullRequestEvent ]
    :> Servant.GitHubSignedReqBody '[Servant.JSON] GitHub.PullRequestEvent
    :> Servant.Post '[Servant.JSON] ()
  :<|>
  "health"
    :> Servant.Summary "Health check"
    :> Servant.Get '[Servant.PlainText] Text


onPing
  :: Servant.RepoWebhookEvent
  -> ((), GitHub.PingEvent)
  -> Servant.Handler ()
onPing GitHub.WebhookPingEvent (_, event) =
  putTextLn ("PingEvent: " <> show event)
onPing _ _ = pass


onPullRequest
  :: Servant.RepoWebhookEvent
  -> ((), GitHub.PullRequestEvent)
  -> Servant.Handler ()
onPullRequest GitHub.WebhookPullRequestEvent (_, event) =
  putTextLn ("PullRequstEvent: " <> show event)
onPullRequest _ _ = pass


onHealthCheck :: Servant.Handler Text
onHealthCheck = pure "All good"


server :: Servant.Server WebhookApi
server = onPing
    :<|> onPullRequest
    :<|> onHealthCheck


--------------------------------------------------------------------------------
-- Stupid hack to make servant-github-webhook work
-- https://github.com/tsani/servant-github-webhook/issues/13#issuecomment-408463124
--------------------------------------------------------------------------------


newtype GitHubKey = GitHubKey (forall result. Servant.GitHubKey result)


gitHubKey :: IO ByteString -> GitHubKey
gitHubKey k = GitHubKey (Servant.gitHubKey k)


instance Servant.HasContextEntry '[GitHubKey] (Servant.GitHubKey result) where
    getContextEntry (GitHubKey x :. _) = x
