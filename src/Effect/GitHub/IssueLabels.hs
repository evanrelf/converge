{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Effect.GitHub.IssueLabels
  ( IssueLabels (..)
  , getLabels
  , addLabels
  , replaceAllLabels
  , removeLabel
  , removeAllLabels
  , runIssueLabelsIO
  )
where

import Data.Vector (Vector)
import GitHub.Data as G (Auth, Error, FetchCount, Id, Issue, IssueLabel, Name, Owner, Repo)
import GitHub.Endpoints.Issues.Labels
  ( addLabelsToIssueR
  , labelsOnIssueR
  , removeAllLabelsFromIssueR
  , removeLabelFromIssueR
  , replaceAllLabelsForIssueR
  )
import GitHub.Request (github)
import Polysemy
import Polysemy.Error as P (Error, fromEither, runError)


data IssueLabels m a where
  GetLabels :: Id Issue -> FetchCount -> IssueLabels m (Vector IssueLabel)
  AddLabels :: Foldable f => Id Issue -> f (Name IssueLabel) -> IssueLabels m (Vector IssueLabel)
  ReplaceAllLabels :: Foldable f => Id Issue -> f (Name IssueLabel) -> IssueLabels m (Vector IssueLabel)
  RemoveLabel :: Id Issue -> Name IssueLabel -> IssueLabels m ()
  RemoveAllLabels :: Id Issue -> IssueLabels m ()

makeSem ''IssueLabels


up
  :: MonadIO m
  => Members '[P.Error e, Embed m] r
  => IO (Either e a)
  -> Sem r a
up = fromEither <=< embed . liftIO


runIssueLabelsIO
  :: MonadIO m
  => Member (Embed m) r
  => Auth
  -> Name Owner
  -> Name Repo
  -> Sem (IssueLabels ': r) a
  -> Sem r (Either G.Error a)
runIssueLabelsIO auth owner repo
  = runError
  . reinterpret \case
    GetLabels issueId fetchCount -> up . github auth $
      labelsOnIssueR owner repo issueId fetchCount

    AddLabels issueId issueLabelNames -> up . github auth $
      addLabelsToIssueR owner repo issueId issueLabelNames

    ReplaceAllLabels issueId issueLabelNames -> up . github auth $
      replaceAllLabelsForIssueR owner repo issueId issueLabelNames

    RemoveLabel issueId issueLabelName -> up . github auth $
      removeLabelFromIssueR owner repo issueId issueLabelName

    RemoveAllLabels issueId -> up . github auth $
      removeAllLabelsFromIssueR owner repo issueId
