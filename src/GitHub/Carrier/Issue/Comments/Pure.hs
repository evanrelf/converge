{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module GitHub.Carrier.Issue.Comments.Pure
  ( IssueCommentsState (..)
  , IssueCommentsPureC (..)
  , runIssueCommentsPure
  -- Re-exports
  , module GitHub.Effect.Issue.Comments
  )
where


import Control.Algebra
import Control.Carrier.Fresh.Strict (Fresh, evalFresh, fresh)
import Control.Carrier.State.Strict (State, get, gets, modify, put, runState)
import Control.Carrier.Throw.Either (Throw, runThrow, throwError)
import Data.Generics.Product.Fields (field)
import Data.Time.Clock (UTCTime)
import qualified GitHub.Data as Data
import Optics (view)
import qualified Relude.Extra.Map as Map

import GitHub.Effect.Issue.Comments


data IssueCommentsState = IssueCommentsState
  { issues :: Map Data.IssueNumber (Set (Data.Id Data.Comment))
  , comments :: Map (Data.Id Data.Comment) Data.IssueComment
  } deriving stock (Generic, Show)


instance Semigroup IssueCommentsState where
  lhs <> rhs = IssueCommentsState
    { issues = issues lhs <> issues rhs
    , comments = comments lhs <> comments rhs
    }


instance Monoid IssueCommentsState where
  mempty = IssueCommentsState
    { issues = mempty
    , comments = mempty
    }


data Error
  = CommentNotFound (Data.Id Data.Comment)
  | IssueNotFound Data.IssueNumber
  deriving stock (Eq, Show)


runIssueCommentsPure
  :: Functor m
  => IssueCommentsState
  -> _m a
  -> m (IssueCommentsState, Either Error a)
runIssueCommentsPure initialState (IssueCommentsPureC m)
  = runState initialState
  . runThrow
  . evalFresh 1
  $ m


newtype IssueCommentsPureC m a = IssueCommentsPureC (m a)
  deriving stock Show
  deriving newtype (Functor, Applicative, Monad)


instance
  ( Algebra sig m
  , Has (State IssueCommentsState) sig m
  , Has (Throw Error) sig m
  , Has Fresh sig m
  )
  => Algebra (IssueComments :+: sig) (IssueCommentsPureC m) where
  alg (R other) = IssueCommentsPureC (alg (handleCoercible other))
  alg (L effect) = do

    case effect of
      GetComment commentId k -> do
        comments <- gets @IssueCommentsState (view (field @"comments"))
        case Map.lookup commentId comments of
          Nothing -> throwError @Error (CommentNotFound commentId)
          Just comment -> k comment

      GetComments issueNumber fetchCount k -> do
        issues <- gets @IssueCommentsState (view (field @"issues"))
        case Map.lookup issueNumber issues of
          Nothing -> throwError @Error (IssueNotFound issueNumber)
          Just issue -> undefined

      CreateComment issueNumber body k -> do
        id <- fresh
        let updatedAt = undefined
        let user = Data.SimpleUser
              { Data.simpleUserId = Data.mkId (Proxy :: _ Data.User) 0
              , Data.simpleUserLogin = undefined
              , Data.simpleUserAvatarUrl = Data.URL "https://example.com"
              , Data.simpleUserUrl = Data.URL "https://example.com"
              }
        let url = Data.URL "https://example.com"
        let htmlUrl = Data.URL "https://example.com"
        let createdAt = undefined
        let issueComment = Data.IssueComment
              { Data.issueCommentUpdatedAt = updatedAt
              , Data.issueCommentUser = user
              , Data.issueCommentUrl = url
              , Data.issueCommentHtmlUrl = htmlUrl
              , Data.issueCommentCreatedAt = undefined
              , Data.issueCommentBody = body
              , Data.issueCommentId = id
              }
        let comment = Data.Comment
              { Data.commentPosition = Nothing
              , Data.commentLine = Nothing
              , Data.commentBody = body
              , Data.commentCommitId = Nothing
              , Data.commentUpdatedAt = updatedAt
              , Data.commentHtmlUrl = Just htmlUrl
              , Data.commentUrl = url
              , Data.commentCreatedAt = createdAt
              , Data.commentPath = Nothing
              , Data.commentUser = user
              , Data.commentId = Data.mkId (Proxy :: _ Data.Comment) id
              }
        undefined
        k comment

      DeleteComment commentId k -> do
        IssueCommentsState{issues, comments} <- get @IssueCommentsState
        let newIssues = undefined
        let newComments = undefined
        put @IssueCommentsState $ IssueCommentsState
          { issues = newIssues
          , comments = newComments
          }
        undefined

      EditComment commentId body k -> do
        undefined
