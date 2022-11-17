module Model.Database
  ( Database
  , getTaskLists
  , getTasksFromTaskList
  , createTaskList
  , deleteTaskList
  , createTask
  , markTask
  ) where

import Control.Lens (At (at), _2, view, (%~), (&), (.~), (<&>), (^?))
import Control.Lens.Prism (_Just)
import Data.Bifunctor (second)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)

import Model.Tasks (TaskList (..), TaskStatus (..), taskListName, tasks)

type Database = Map Integer TaskList

getTaskLists :: Database -> [(Integer, Text)]
getTaskLists database = database
  & M.toList
  <&> second (view taskListName)

getTasksFromTaskList :: Integer -> Database -> Maybe (Map Integer (Text, TaskStatus))
getTasksFromTaskList listId database = database ^? at listId . _Just . tasks

createTaskList :: Integer -> Text -> Database -> Database
createTaskList listId name = M.insert listId (TaskList name M.empty)

deleteTaskList :: Integer -> Database -> Database
deleteTaskList = M.delete

createTask :: Integer -> Integer -> Text -> Database -> Database
createTask listId taskId name database =
  database & at listId . _Just . tasks %~ \taskList ->
    if taskId `M.member` taskList
    then taskList
    else M.insert taskId (name, NotDone) taskList

markTask :: Integer -> Integer -> TaskStatus -> Database -> Database
markTask listId taskId status database =
  database & at listId . _Just . tasks . at taskId . _Just . _2 .~ status
