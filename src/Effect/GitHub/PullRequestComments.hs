{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Effect.GitHub.PullRequestComments
  ( PullRequestComments (..)
  , comment
  , comments
  , createComment
  , pullRequestCommentsToIO
  )
where

import Data.Vector (Vector)
import qualified GitHub.Data as G
import qualified GitHub.Endpoints.PullRequests.Comments as Fns
import GitHub.Request (github)
import Polysemy
import Polysemy.Error (Error, fromEither, runError)


data PullRequestComments m a where
  Comment :: G.Id G.Comment -> PullRequestComments m G.Comment
  Comments :: G.IssueNumber -> G.FetchCount -> PullRequestComments m (Vector G.Comment)
  CreateComment :: G.IssueNumber -> Text -> Text -> Int -> Text -> PullRequestComments m G.Comment

makeSem ''PullRequestComments


up
  :: MonadIO m
  => Members '[Error e, Embed m] r
  => IO (Either e a)
  -> Sem r a
up = fromEither <=< embed . liftIO


pullRequestCommentsToIO
  :: MonadIO m
  => Member (Embed m) r
  => G.Auth
  -> G.Name G.Owner
  -> G.Name G.Repo
  -> Sem (PullRequestComments ': r) a
  -> Sem r (Either G.Error a)
pullRequestCommentsToIO auth owner repo
  = runError
  . reinterpret \case
    Comment commentId -> up . github auth $
      Fns.pullRequestCommentR owner repo commentId

    Comments issueNumber fetchCount -> up . github auth $
      Fns.pullRequestCommentsR owner repo issueNumber fetchCount

    CreateComment issueNumber commit path position body -> up . github auth $
      Fns.createPullCommentR owner repo issueNumber commit path position body
