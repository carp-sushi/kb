{-# LANGUAGE BlockArguments #-}

module Service where

import Dto
import Foundation
import Model

import Database.Persist.Sql
import Yesod.Core
import Yesod.Persist.Core (get404, runDB)

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
