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
import GitHub.Data as G (Auth, Comment, Error, FetchCount, Id, IssueComment, IssueNumber, Name, Owner, Repo)
import qualified GitHub.Endpoints.Issues.Comments as Fns
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
      Fns.commentR owner repo commentId

    GetComments issueNumber fetchCount -> up . github auth $
      Fns.commentsR owner repo issueNumber fetchCount

    CreateComment issueNumber body -> up . github auth $
      Fns.createCommentR owner repo issueNumber body

    DeleteComment commentId -> up . github auth $
      Fns.deleteCommentR owner repo commentId

    EditComment commentId body -> up . github auth $
      Fns.editCommentR owner repo commentId body
