module Model.Tasks
  ( TaskStatus (..)
  , getOppositeStatus
  , TaskList (..)
  , taskListName
  , tasks
  ) where

import Control.Lens (abbreviatedFields, makeLensesWith)
import Data.Map (Map)
import Data.Text (Text)
import Text.Blaze (ToMarkup (toMarkup), text)

data TaskStatus
  = NotDone
  | Done
  deriving stock (Show, Eq, Ord)

getOppositeStatus :: TaskStatus -> TaskStatus
getOppositeStatus = \case
  NotDone -> Done
  Done -> NotDone

instance ToMarkup TaskStatus where
  toMarkup = \case
    NotDone -> text "Not done"
    Done -> text "Done"

data TaskList = TaskList
  { tlTaskListName :: Text
  , tlTasks :: Map Integer (Text, TaskStatus)
  }
  deriving stock (Show, Eq, Ord)

makeLensesWith abbreviatedFields ''TaskList


