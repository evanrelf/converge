{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Converge
  ( WebhookApi
  , server
  , gitHubKey
  )
where

import GHC.TypeLits (Symbol)
import qualified GitHub.Data as GitHub
import Servant ((:<|>) (..), (:>), Context ((:.)))
import qualified Servant
import qualified Servant.GitHub.Webhook as Servant


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
    'GitHub.WebhookPingEvent GitHub.PingEvent
    :<|>
  Webhook "Pull request event from GitHub"
    'GitHub.WebhookPullRequestEvent GitHub.PullRequestEvent


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
