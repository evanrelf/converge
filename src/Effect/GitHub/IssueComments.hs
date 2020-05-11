{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Effect.GitHub.IssueComments
  ( IssueComments (..)
  , comment
  , comments
  , createComment
  , deleteComment
  , editComment
  , issueCommentsToIO
  )
where

import Data.Vector (Vector)
import qualified GitHub.Data as G
import qualified GitHub.Endpoints.Issues.Comments as Fns
import GitHub.Request (github)
import Polysemy
import Polysemy.Error (Error, fromEither, runError)


data IssueComments m a where
  Comment :: G.Id G.Comment -> IssueComments m G.IssueComment
  Comments :: G.IssueNumber -> G.FetchCount -> IssueComments m (Vector G.IssueComment)
  CreateComment :: G.IssueNumber -> Text -> IssueComments m G.Comment
  DeleteComment :: G.Id G.Comment -> IssueComments m ()
  EditComment :: G.Id G.Comment -> Text -> IssueComments m G.Comment

makeSem ''IssueComments


up
  :: Members '[Error e, Embed IO] r
  => IO (Either e a)
  -> Sem r a
up = fromEither <=< embed


issueCommentsToIO
  :: Member (Embed IO) r
  => G.Auth
  -> G.Name G.Owner
  -> G.Name G.Repo
  -> Sem (IssueComments ': r) a
  -> Sem r (Either G.Error a)
issueCommentsToIO auth owner repo
  = runError
  . reinterpret \case
    Comment commentId -> up . github auth $
      Fns.commentR owner repo commentId

    Comments issueNumber fetchCount -> up . github auth $
      Fns.commentsR owner repo issueNumber fetchCount

    CreateComment issueNumber body -> up . github auth $
      Fns.createCommentR owner repo issueNumber body

    DeleteComment commentId -> up . github auth $
      Fns.deleteCommentR owner repo commentId

    EditComment commentId body -> up . github auth $
      Fns.editCommentR owner repo commentId body
