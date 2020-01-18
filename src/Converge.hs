{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Converge (main) where

import qualified GitHub.Data as GitHub
import qualified Network.Wai.Handler.Warp as Warp
import Servant ((:>), Context((:.)))
import qualified Servant
import qualified Servant.GitHub.Webhook as W

type Api =
  "webhook"
    :> Servant.Summary "Ping"
    :> W.GitHubEvent '[ 'GitHub.WebhookPingEvent ]
    :> W.GitHubSignedReqBody '[Servant.JSON] GitHub.PingEvent
    :> Servant.Post '[Servant.JSON] ()

onPing
  :: W.RepoWebhookEvent
  -> ((), GitHub.PingEvent)
  -> Servant.Handler ()
onPing GitHub.WebhookPingEvent (_, event) =
  putTextLn ("PingEvent: " <> show event)
onPing _ _ = pass

server :: Servant.Server Api
server = onPing

main :: IO ()
main = do
  let host = "localhost"
  let port = 8080
  let secret = "super-secret-shhh-dont-tell" :: ByteString
  let context = gitHubKey (pure secret) :. Servant.EmptyContext
  putTextLn ("Running at http://" <> host <> ":" <> show port)
  Warp.run port (Servant.serveWithContext (Proxy @Api) context server)

-- Stupid hack to make servant-github-webhook work
-- https://github.com/tsani/servant-github-webhook/issues/13#issuecomment-408463124

newtype GitHubKey = GitHubKey (forall result. W.GitHubKey result)

gitHubKey :: IO ByteString -> GitHubKey
gitHubKey k = GitHubKey (W.gitHubKey k)

instance Servant.HasContextEntry '[GitHubKey] (W.GitHubKey result) where
    getContextEntry (GitHubKey x :. _) = x
