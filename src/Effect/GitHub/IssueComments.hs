{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Effect.GitHub.IssueComments
  ( IssueComments (..)
  , getComment
  , getComments
  , createComment
  , deleteComment
  , editComment
  , runIssueCommentsIO
  )
where

import Data.Vector (Vector)
import GitHub.Data (Comment, FetchCount, Id, IssueComment, IssueNumber)
import GitHub.Data as G (Auth, Error, Name, Owner, Repo)
import GitHub.Endpoints.Issues.Comments
  ( commentR
  , commentsR
  , createCommentR
  , deleteCommentR
  , editCommentR
  )
import GitHub.Request (github)
import Polysemy
import Polysemy.Error as P (Error, fromEither, runError)


data IssueComments m a where
  GetComment :: Id Comment -> IssueComments m IssueComment
  GetComments :: IssueNumber -> FetchCount -> IssueComments m (Vector IssueComment)
  CreateComment :: IssueNumber -> Text -> IssueComments m Comment
  DeleteComment :: Id Comment -> IssueComments m ()
  EditComment :: Id Comment -> Text -> IssueComments m Comment

makeSem ''IssueComments


up
  :: MonadIO m
  => Members '[P.Error e, Embed m] r
  => IO (Either e a)
  -> Sem r a
up = fromEither <=< embed . liftIO


runIssueCommentsIO
  :: MonadIO m
  => Member (Embed m) r
  => Auth
  -> Name Owner
  -> Name Repo
  -> Sem (IssueComments ': r) a
  -> Sem r (Either G.Error a)
runIssueCommentsIO auth owner repo
  = runError
  . reinterpret \case
    GetComment commentId -> up . github auth $
      commentR owner repo commentId

    GetComments issueNumber fetchCount -> up . github auth $
      commentsR owner repo issueNumber fetchCount

    CreateComment issueNumber body -> up . github auth $
      createCommentR owner repo issueNumber body

    DeleteComment commentId -> up . github auth $
      deleteCommentR owner repo commentId

    EditComment commentId body -> up . github auth $
      editCommentR owner repo commentId body
