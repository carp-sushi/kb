{-# LANGUAGE OverloadedStrings #-}

module Handler where

import Foundation
import Model
import Page
import Repo
import Validate

import Control.Monad ((>=>))
import Yesod.Core

-- | Get a page of boards.
getBoardsR :: Handler Value
getBoardsR =
    queryPage listBoards

-- | Create a board.
postBoardsR :: Handler Value
postBoardsR = do
    body <- requireCheckJsonBody :: Handler Board
    case validateBoard body of
        Left errors -> invalidArgs errors
        Right board -> (createBoard >=> returnJson) board

-- | Get a board.
getBoardR :: BoardId -> Handler Value
getBoardR boardId =
    lookupBoard boardId >>= returnJson

-- | Update a board.
putBoardR :: BoardId -> Handler Value
putBoardR boardId = do
    body <- requireCheckJsonBody :: Handler Board
    case validateBoard body of
        Left errors -> invalidArgs errors
        Right board -> (updateBoard boardId >=> returnJson) board

-- | Delete a board and all tasks on the board.
deleteBoardR :: BoardId -> Handler ()
deleteBoardR boardId =
    deleteBoard boardId >> sendResponseNoContent

-- | Get all tasks on a board.
getBoardTasksR :: BoardId -> Handler Value
getBoardTasksR boardId =
    queryPage $ listTasks boardId

-- | Move a task to a board.
postBoardTasksR :: BoardId -> Handler Value
postBoardTasksR boardId = do
    taskId <- requireCheckJsonBody :: Handler TaskId
    task <- moveTaskToBoard boardId taskId
    returnJson task

-- | Create a task.
postTasksR :: Handler Value
postTasksR = do
    body <- requireCheckJsonBody :: Handler Task
    case validateTask body of
        Left errors -> invalidArgs errors
        Right task -> (createTask >=> returnJson) task

-- | Get a task.
getTaskR :: TaskId -> Handler Value
getTaskR taskId =
    lookupTask taskId
        >>= returnJson

-- | Update a task.
putTaskR :: TaskId -> Handler Value
putTaskR taskId = do
    body <- requireCheckJsonBody :: Handler Task
    case validateTask body of
        Left errors -> invalidArgs errors
        Right task -> (updateTask taskId >=> returnJson) task

-- | Delete a task.
deleteTaskR :: TaskId -> Handler ()
deleteTaskR taskId =
    deleteTask taskId >> sendResponseNoContent

-- | Get all milestones linked to a task.
getTaskMilestonesR :: TaskId -> Handler Value
getTaskMilestonesR taskId =
    queryPage $ listTaskMilestones taskId

-- | List all milestones.
getMilestonesR :: Handler Value
getMilestonesR =
    queryPage listMilestones

-- | Create a milestone.
postMilestonesR :: Handler Value
postMilestonesR = do
    body <- requireCheckJsonBody :: Handler Milestone
    case validateMilestone body of
        Left errors -> invalidArgs errors
        Right milestone -> (createMilestone >=> returnJson) milestone

-- | Get a milestone.
getMilestoneR :: MilestoneId -> Handler Value
getMilestoneR milestoneId =
    lookupMilestone milestoneId >>= returnJson

putMilestoneR :: MilestoneId -> Handler Value
putMilestoneR milestoneId = do
    body <- requireCheckJsonBody :: Handler Milestone
    case validateMilestone body of
        Left errors -> invalidArgs errors
        Right milestone -> (updateMilestone milestoneId >=> returnJson) milestone

-- | Delete a milestone.
deleteMilestoneR :: MilestoneId -> Handler ()
deleteMilestoneR milestoneId =
    deleteMilestone milestoneId >> sendResponseNoContent

-- | Get all tasks linked to a milestone.
getMilestoneTasksR :: MilestoneId -> Handler Value
getMilestoneTasksR milestoneId =
    queryPage $ listMilestoneTasks milestoneId

-- | Link a milestone to a task
postMilestoneTasksR :: MilestoneId -> Handler Value
postMilestoneTasksR milestoneId =
    (requireCheckJsonBody :: Handler TaskId)
        >>= createMilestoneTask milestoneId
        >>= returnJson

-- | Unlink a milestone from a task
deleteMilestoneTaskR :: MilestoneId -> TaskId -> Handler ()
deleteMilestoneTaskR milestoneId taskId =
    deleteMilestoneTask milestoneId taskId >> sendResponseNoContent

-- Helper: run a page query and return as JSON.
queryPage :: (ToJSON a) => (Int -> Int -> Handler [a]) -> Handler Value
queryPage pageQuery = do
    pageParams@(PageParams size number) <- readPageParams
    pageQuery size (size * (number - 1))
        >>= pageJson pageParams

-- | Create a JSON data transfer object for a page.
pageJson :: (ToJSON a) => PageParams -> [a] -> Handler Value
pageJson (PageParams size number) pageData =
    returnJson $
        object
            [ "pageSize" .= size
            , "previousPageNumber" .= max 1 (number - 1)
            , "pageNumber" .= number
            , "nextPageNumber" .= (number + 1)
            , "pageData" .= pageData
            ]
