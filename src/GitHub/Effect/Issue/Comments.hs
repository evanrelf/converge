module GitHub.Effect.Issue.Comments
  ( IssueComments (..)
  , getComment
  , getComments
  , createComment
  , deleteComment
  , editComment
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
import GitHub.Data (Comment, FetchCount, Id, IssueComment, IssueNumber)


data IssueComments m k
  = GetComment (Id Comment) (IssueComment -> m k)
  | GetComments IssueNumber FetchCount (Vector IssueComment -> m k)
  | CreateComment IssueNumber Text (Comment -> m k)
  | DeleteComment (Id Comment) (m k)
  | EditComment (Id Comment) Text (Comment -> m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)


getComment :: Has IssueComments sig m => Id Comment -> m IssueComment
getComment commentId = send (GetComment commentId pure)


getComments
  :: Has IssueComments sig m
  => IssueNumber
  -> FetchCount
  -> m (Vector IssueComment)
getComments issueNumber fetchCount =
  send (GetComments issueNumber fetchCount pure)


createComment :: Has IssueComments sig m => IssueNumber -> Text -> m Comment
createComment issueNumber body = send (CreateComment issueNumber body pure)


deleteComment :: Has IssueComments sig m => Id Comment -> m ()
deleteComment commentId = send (DeleteComment commentId pass)


editComment :: Has IssueComments sig m => Id Comment -> Text -> m Comment
editComment commentId body = send (EditComment commentId body pure)
