module Converge.State where


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
  } deriving stock Show
