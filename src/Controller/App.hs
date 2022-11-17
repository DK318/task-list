{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Controller.App
  ( App (..)
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar, readTVar, readTVarIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Map (toList, (!?))
import Data.Text (Text)
import Yesod
  (FieldSettings, FormMessage, FormResult (FormSuccess), Html, MForm, MonadIO (liftIO),
  RenderMessage (..), RenderRoute (renderRoute), Yesod (defaultLayout), areq, defaultFormMessage,
  generateFormPost, getYesod, mkYesod, parseRoutes, redirect, renderDivs, runFormPost, textField,
  whamletFile)

import Model.Database
  (Database, createTask, createTaskList, deleteTaskList, getTaskLists, getTasksFromTaskList,
  markTask)
import Model.Tasks (getOppositeStatus)

data App = App
  { aDatabase :: TVar Database
  , aCounter :: TVar Integer
  }

mkYesod "App" [parseRoutes|
/ IndexR GET
/add AddTaskListR POST
/taskList/#Integer TaskListR GET
/taskList/#Integer/add AddTaskR POST
/taskList/#Integer/deleteList DeleteTaskListR POST
/taskList/#Integer/changeMark/#Integer ChangeMarkR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

textForm :: FieldSettings App -> Html -> MForm Handler (FormResult Text, Widget)
textForm settings = renderDivs
  $ areq textField settings Nothing

getIndexR :: Handler Html
getIndexR = do
  App{..} <- getYesod
  taskLists <- getTaskLists <$> liftIO (readTVarIO aDatabase)

  (widget, enctype) <- generateFormPost (textForm "Add task list")

  defaultLayout $(whamletFile "src/resourses/index.hamlet")

postAddTaskListR :: Handler Html
postAddTaskListR = do
  ((result, _), _) <- runFormPost (textForm "Add task list")
  case result of
    FormSuccess taskListName -> do
      App{..} <- getYesod

      liftIO $ atomically do
        lastId <- readTVar aCounter
        modifyTVar aDatabase (createTaskList lastId taskListName)
        modifyTVar aCounter (+1)

      redirect IndexR
    _ -> redirect IndexR

postAddTaskR :: Integer -> Handler Html
postAddTaskR listId = do
  ((result, _), _) <- runFormPost (textForm "Add task")
  case result of
    FormSuccess taskName -> do
      App{..} <- getYesod

      liftIO $ atomically do
        lastId <- readTVar aCounter
        modifyTVar aDatabase (createTask listId lastId taskName)
        modifyTVar aCounter (+1)

      redirect (TaskListR listId)
    _ -> redirect (TaskListR listId)

getChangeMarkR :: Integer -> Integer -> Handler Html
getChangeMarkR listId taskId = do
  App{..} <- getYesod

  statusMb <-
    runMaybeT do
      Just tasks <- liftIO (getTasksFromTaskList listId <$> readTVarIO aDatabase)
      MaybeT . pure $ tasks !? taskId

  case statusMb of
    Just (_, status) -> do
      liftIO $ atomically do
        modifyTVar aDatabase (markTask listId taskId (getOppositeStatus status))

      redirect (TaskListR listId)
    Nothing -> redirect IndexR

postDeleteTaskListR :: Integer -> Handler Html
postDeleteTaskListR listId = do
  App{..} <- getYesod

  liftIO $ atomically do
    modifyTVar aDatabase (deleteTaskList listId)

  redirect IndexR

getTaskListR :: Integer -> Handler Html
getTaskListR listId = do
  App{..} <- getYesod
  tasksMb <- getTasksFromTaskList listId <$> liftIO (readTVarIO aDatabase)

  (widget, enctype) <- generateFormPost (textForm "Add task")

  defaultLayout $(whamletFile "src/resourses/allTasks.hamlet")
