module GitHub.Effect.PullRequest.Comments
  ( PullRequestComments (..)
  , getComment
  , getComments
  , createComment
  -- Re-exports
  , Algebra
  , Effect
  , Has
  , run
  )
where


import Control.Algebra (Algebra, Effect, HFunctor, Has, run, send)
import Data.Vector (Vector)
import GHC.Generics (Generic1)
import GitHub.Data (Comment, FetchCount, Id, IssueNumber)


data PullRequestComments m k
  = GetComment (Id Comment) (Comment -> m k)
  | GetComments IssueNumber FetchCount (Vector Comment -> m k)
  | CreateComment IssueNumber Text Text Int Text (Comment -> m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)


getComment :: Has PullRequestComments sig m => Id Comment -> m Comment
getComment commentId = send (GetComment commentId pure)


getComments
  :: Has PullRequestComments sig m
  => IssueNumber
  -> FetchCount
  -> m (Vector Comment)
getComments issueNumber fetchCount =
  send (GetComments issueNumber fetchCount pure)


createComment
  :: Has PullRequestComments sig m
  => IssueNumber
  -> Text
  -> Text
  -> Int
  -> Text
  -> m Comment
createComment issueNumber commit path position body =
  send (CreateComment issueNumber commit path position body pure)
