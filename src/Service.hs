{-# LANGUAGE BlockArguments #-}

module Service where

import Dto
import Foundation
import Model

import Database.Persist.Sql
import Yesod.Core
import Yesod.Persist.Core (get404, runDB)

-- | List all boards as a JSON array.
listBoards :: Handler Value
listBoards = do
    boards <- runDB $ selectList [] [Asc BoardPosition, Asc BoardName]
    returnJson boards

-- | Lookup a board by id.
lookupBoard :: BoardId -> Handler Value
lookupBoard boardId = do
    board <- runDB $ get404 boardId
    returnJson $ boardDto boardId board

-- | Insert a board in the database and return it as a JSON value.
createBoard :: Board -> Handler Value
createBoard board = do
    inserted <- runDB $ insertEntity board
    returnJson inserted

-- | Update a board in the database and return it as a JSON value.
updateBoard :: BoardId -> Board -> Handler Value
updateBoard boardId board = do
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
deleteBoard :: BoardId -> Handler ()
deleteBoard boardId = runDB $ do
    _ <- get404 boardId
    deleteWhere [TaskBoardId ==. boardId]
    delete boardId

-- | List all tasks on a board as a JSON array.
listTasks :: BoardId -> Handler Value
listTasks boardId = do
    tasks <- runDB $ selectList [TaskBoardId ==. boardId] [Asc TaskId]
    returnJson tasks

-- | Lookup a task by id.
lookupTask :: TaskId -> Handler Value
lookupTask taskId = do
    task <- runDB $ get404 taskId
    returnJson $ taskDto taskId task

-- | Insert a task in the database and return it as a JSON value.
createTask :: Task -> Handler Value
createTask task = do
    inserted <- runDB $ do
        _ <- get404 $ taskBoardId task
        insertEntity task
    returnJson inserted

-- | Ipdate a task in the database and return it as a JSON value.
updateTask :: TaskId -> Task -> Handler Value
updateTask taskId task = do
    updated <- runDB $ do
        _ <- get404 $ taskBoardId task
        update
            taskId
            [ TaskBoardId =. taskBoardId task
            , TaskName =. taskName task
            , TaskPoints =. taskPoints task
            , TaskStatus =. taskStatus task
            ]
        get404 taskId
    returnJson $ taskDto taskId updated

-- | Delete a task.
deleteTask :: TaskId -> Handler ()
deleteTask taskId = runDB $ do
    _ <- get404 taskId
    delete taskId
