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
import qualified GitHub.Data as G
import qualified GitHub.Endpoints.Issues.Labels as Fns
import GitHub.Request (github)
import Polysemy
import Polysemy.Error (Error, fromEither, runError)


data IssueLabels m a where
  GetLabels :: G.Id G.Issue -> G.FetchCount -> IssueLabels m (Vector G.IssueLabel)
  AddLabels :: Foldable f => G.Id G.Issue -> f (G.Name G.IssueLabel) -> IssueLabels m (Vector G.IssueLabel)
  ReplaceAllLabels :: Foldable f => G.Id G.Issue -> f (G.Name G.IssueLabel) -> IssueLabels m (Vector G.IssueLabel)
  RemoveLabel :: G.Id G.Issue -> G.Name G.IssueLabel -> IssueLabels m ()
  RemoveAllLabels :: G.Id G.Issue -> IssueLabels m ()

makeSem ''IssueLabels


up
  :: MonadIO m
  => Members '[Error e, Embed m] r
  => IO (Either e a)
  -> Sem r a
up = fromEither <=< embed . liftIO


runIssueLabelsIO
  :: MonadIO m
  => Member (Embed m) r
  => G.Auth
  -> G.Name G.Owner
  -> G.Name G.Repo
  -> Sem (IssueLabels ': r) a
  -> Sem r (Either G.Error a)
runIssueLabelsIO auth owner repo
  = runError
  . reinterpret \case
    GetLabels issueId fetchCount -> up . github auth $
      Fns.labelsOnIssueR owner repo issueId fetchCount

    AddLabels issueId issueLabelNames -> up . github auth $
      Fns.addLabelsToIssueR owner repo issueId issueLabelNames

    ReplaceAllLabels issueId issueLabelNames -> up . github auth $
      Fns.replaceAllLabelsForIssueR owner repo issueId issueLabelNames

    RemoveLabel issueId issueLabelName -> up . github auth $
      Fns.removeLabelFromIssueR owner repo issueId issueLabelName

    RemoveAllLabels issueId -> up . github auth $
      Fns.removeAllLabelsFromIssueR owner repo issueId
