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
import Control.Carrier.Lift (Lift, runM, sendM)
import Control.Carrier.Reader (Reader, ask, runReader)
import Control.Carrier.Throw.Either (Throw, runThrow, throwError)
import GHC.TypeLits (Symbol)
import qualified GitHub.Data as Data
import qualified GitHub.Endpoints.Issues.Comments as Comments
import qualified GitHub.Request as Request
import Servant ((:<|>) (..), (:>), Context ((:.)))
import qualified Servant
import qualified Servant.GitHub.Webhook as Servant


--------------------------------------------------------------------------------
-- Scratchpad
--------------------------------------------------------------------------------


type GitHub sig m =
  ( Has (Reader Data.Auth) sig m
  , Has (Throw Data.Error) sig m
  , Has (Lift IO) sig m
  )


runGitHub :: MonadIO m => Data.Auth -> _m a -> m (Either Data.Error a)
runGitHub auth = runM . runThrow . runReader auth


leaveComment
  :: GitHub sig m
  => Data.Name Data.Owner
  -> Data.Name Data.Repo
  -> Data.IssueNumber
  -> Text
  -> m ()
leaveComment owner repo issueNumber body = do
  auth <- ask @Data.Auth
  result <- sendM $ Request.github auth (Comments.createCommentR owner repo issueNumber body)
  whenLeft_ result throwError


-- Works!
test :: ByteString -> IO ()
test token = do
  -- I just used a personal access token
  let auth = Data.OAuth token
  result <- runGitHub auth $ do
    let owner = "evanrelf"
        repo = "github-apps-test"
        issueNumber = Data.IssueNumber 1
        body = "Hello world!"
    leaveComment owner repo issueNumber body
  print result


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
