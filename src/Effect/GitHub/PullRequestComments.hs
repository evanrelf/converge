{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Effect.GitHub.PullRequestComments
  ( PullRequestComments (..)
  , getComment
  , getComments
  , createComment
  , runPullRequestCommentsIO
  )
where

import Data.Vector (Vector)
import GitHub.Data as G (Auth, Comment, Error, FetchCount, Id, IssueNumber, Name, Owner, Repo)
import qualified GitHub.Endpoints.PullRequests.Comments as Fns
import GitHub.Request (github)
import Polysemy
import Polysemy.Error as P (Error, fromEither, runError)


data PullRequestComments m a where
  GetComment :: Id Comment -> PullRequestComments m Comment
  GetComments :: IssueNumber -> FetchCount -> PullRequestComments m (Vector Comment)
  CreateComment :: IssueNumber -> Text -> Text -> Int -> Text -> PullRequestComments m Comment

makeSem ''PullRequestComments


up
  :: MonadIO m
  => Members '[P.Error e, Embed m] r
  => IO (Either e a)
  -> Sem r a
up = fromEither <=< embed . liftIO


runPullRequestCommentsIO
  :: MonadIO m
  => Member (Embed m) r
  => Auth
  -> Name Owner
  -> Name Repo
  -> Sem (PullRequestComments ': r) a
  -> Sem r (Either G.Error a)
runPullRequestCommentsIO auth owner repo
  = runError
  . reinterpret \case
    GetComment commentId -> up . github auth $
      Fns.pullRequestCommentR owner repo commentId

    GetComments issueNumber fetchCount -> up . github auth $
      Fns.pullRequestCommentsR owner repo issueNumber fetchCount

    CreateComment issueNumber commit path position body -> up . github auth $
      Fns.createPullCommentR owner repo issueNumber commit path position body
