module Converge.State where

import Generic.Data (Generically (..))


data PullRequestState
  = Draft
  | Open
  | Merged
  | Closed
  deriving stock Show


data PullRequest = PullRequest
  { state :: PullRequestState
  , baseBranch :: Text
  , headBranch :: Text
  } deriving stock Show


data World = World
  { pullRequests :: Map Natural PullRequest
  } deriving stock (Generic, Show)
    deriving (Semigroup, Monoid) via Generically World
