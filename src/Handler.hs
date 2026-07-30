{-# LANGUAGE BlockArguments #-}

module Handler where

import Dto
import Foundation
import Model

import Database.Persist.Sql
import Yesod.Core
import Yesod.Persist.Core (get404, runDB)

-- | List all boards ordered by position.
getBoardsR :: Handler Value
getBoardsR = do
    boards <- runDB $ selectList [] [Asc BoardPosition]
    returnJson boards

-- | Create a board.
postBoardsR :: Handler Value
postBoardsR = do
    board <- requireCheckJsonBody :: Handler Board
    inserted <- runDB $ insertEntity board
    returnJson inserted

-- | Get a board.
getBoardR :: BoardId -> Handler Value
getBoardR boardId =
    runDB (get404 boardId)
        >>= returnJson . boardDto boardId

-- | Update a board.
putBoardR :: BoardId -> Handler Value
putBoardR boardId = do
    board <- requireCheckJsonBody :: Handler Board
    updated <- runDB $ do
        update
            boardId
            [ BoardName =. boardName board
            , BoardColor =. boardColor board
            , BoardPosition =. boardPosition board
            ]
        get404 boardId
    returnJson $ boardDto boardId updated

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
    task <- requireCheckJsonBody :: Handler Task
    inserted <- runDB $ do
        _ <- get404 $ taskBoardId task
        insertEntity task
    returnJson inserted

-- | Get a task.
getTaskR :: TaskId -> Handler Value
getTaskR taskId = do
    runDB (get404 taskId)
        >>= returnJson . taskDto taskId

-- | Update a task.
putTaskR :: TaskId -> Handler Value
putTaskR taskId = do
    task <- requireCheckJsonBody :: Handler Task
    runDB $ do
        _ <- get404 taskId
        _ <- get404 $ taskBoardId task
        update
            taskId
            [ TaskBoardId =. taskBoardId task
            , TaskName =. taskName task
            , TaskPoints =. taskPoints task
            , TaskStatus =. taskStatus task
            ]
    returnJson $ taskDto taskId task

-- | Delete a task.
deleteTaskR :: TaskId -> Handler ()
deleteTaskR taskId =
    runDB $ do
        _ <- get404 taskId
        delete taskId
