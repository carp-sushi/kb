{-# LANGUAGE BlockArguments #-}

module Handler where

import Dto
import Foundation
import Model
import Service
import Validate

import Database.Persist.Sql
import Yesod.Core
import Yesod.Persist.Core (get404, runDB)

-- | List all boards ordered by position.
getBoardsR :: Handler Value
getBoardsR = do
    boards <- runDB $ selectList [] [Asc BoardPosition, Asc BoardName]
    returnJson boards

-- | Create a board.
postBoardsR :: Handler Value
postBoardsR = do
    body <- requireCheckJsonBody :: Handler Board
    either invalidArgs createBoard $ validateBoard body

-- | Get a board.
getBoardR :: BoardId -> Handler Value
getBoardR boardId = do
    board <- runDB $ get404 boardId
    returnJson $ boardDto boardId board

-- | Update a board.
putBoardR :: BoardId -> Handler Value
putBoardR boardId = do
    body <- requireCheckJsonBody :: Handler Board
    either invalidArgs (updateBoard boardId) $ validateBoard body

-- | Delete a board and all tasks on the board.
deleteBoardR :: BoardId -> Handler ()
deleteBoardR boardId =
    runDB $ do
        _ <- get404 boardId
        deleteWhere [TaskBoardId ==. boardId]
        delete boardId

-- | Get a page of tasks on a board.
getBoardTasksR :: BoardId -> Handler Value
getBoardTasksR boardId = do
    tasks <- runDB $ selectList [TaskBoardId ==. boardId] [Asc TaskId]
    returnJson tasks

-- | Create a task.
postTasksR :: Handler Value
postTasksR = do
    body <- requireCheckJsonBody :: Handler Task
    either invalidArgs createTask $ validateTask body

-- | Get a task.
getTaskR :: TaskId -> Handler Value
getTaskR taskId = do
    task <- runDB $ get404 taskId
    returnJson $ taskDto taskId task

-- | Update a task.
putTaskR :: TaskId -> Handler Value
putTaskR taskId = do
    body <- requireCheckJsonBody :: Handler Task
    either invalidArgs (updateTask taskId) $ validateTask body

-- | Delete a task.
deleteTaskR :: TaskId -> Handler ()
deleteTaskR taskId =
    runDB $ do
        _ <- get404 taskId
        delete taskId
