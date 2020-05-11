module Converge.State where

import qualified Data.Aeson as Aeson
import Generic.Data (Generically (..))


data PullRequestState
  = Draft
  | Open
  | Merged
  | Closed
  deriving stock (Generic, Show)
  deriving anyclass Aeson.ToJSON


data PullRequest = PullRequest
  { state :: PullRequestState
  , baseBranch :: Text
  , headBranch :: Text
  } deriving stock (Generic, Show)
    deriving anyclass Aeson.ToJSON


data World = World
  { pullRequests :: Map Natural PullRequest
  } deriving stock (Generic, Show)
    deriving (Semigroup, Monoid) via Generically World
    deriving anyclass Aeson.ToJSON
