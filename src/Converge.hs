{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Converge where

import Control.Concurrent.Classy (MonadConc)
import qualified Control.Concurrent.Classy as Concurrent
import qualified Control.Concurrent.Classy.Async as Async
import Converge.Api (Api, gitHubKey, server)
import Data.Generics.Product (field)
import Data.String.Interpolate (i)
import Generic.Data (Generically (..))
import qualified Network.Wai.Handler.Warp as Warp
import Optics ((%), at, ix, over, sans, set)
import Servant (Context ((:.)))
import qualified Servant
import Prelude hiding (id)


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
