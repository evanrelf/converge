{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Control.Carrier.State.Strict (State, gets, modify)
import Control.Carrier.Throw.Either (Throw, throwError)
import qualified GitHub.Data as Data
import Relude.Extra.Map (lookup)

import GitHub.Effect.Issue.Comments


data IssueCommentsState = IssueCommentsState
  { issues :: Map Data.IssueNumber (Set (Data.Id Data.Comment))
  , comments :: Map (Data.Id Data.Comment) Data.IssueComment
  } deriving stock Show


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


runIssueCommentsPure :: IssueCommentsState -> _ -> m (Either Error a)
runIssueCommentsPure initialState = undefined


newtype IssueCommentsPureC m a = IssueCommentsPureC (m a)
  deriving stock Show
  deriving newtype (Functor, Applicative, Monad)


instance
  ( Algebra sig m
  , Has (State IssueCommentsState) sig m
  , Has (Throw Error) sig m
  )
  => Algebra (IssueComments :+: sig) (IssueCommentsPureC m) where
  alg (R other) = IssueCommentsPureC (alg (handleCoercible other))
  alg (L effect) = do

    case effect of
      GetComment commentId k -> do
        comments <- gets @IssueCommentsState comments
        case lookup commentId comments of
          Nothing -> throwError @Error (CommentNotFound commentId)
          Just comment -> k comment

      GetComments issueNumber fetchCount k -> do
        issues <- gets @IssueCommentsState issues
        case lookup issueNumber issues of
          Nothing -> throwError @Error (IssueNotFound issueNumber)
          Just issue -> undefined

      CreateComment issueNumber body k -> do
        undefined

      DeleteComment commentId k -> do
        undefined

      EditComment commentId body k -> do
        undefined
