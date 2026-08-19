{-# LANGUAGE BlockArguments #-}

module Handler where

import Dto
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
    lookupBoard boardId
        >>= returnJson . boardJson boardId

-- | Update a board.
putBoardR :: BoardId -> Handler Value
putBoardR boardId = do
    body <- requireCheckJsonBody :: Handler Board
    case validateBoard body of
        Left errors -> invalidArgs errors
        Right board -> update board
  where
    update =
        updateBoard boardId
            >=> returnJson . boardJson boardId

-- | Delete a board and all tasks on the board.
deleteBoardR :: BoardId -> Handler ()
deleteBoardR =
    deleteBoard

-- | Get all tasks on a board.
getBoardTasksR :: BoardId -> Handler Value
getBoardTasksR boardId =
    queryPage $ listTasks boardId

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
        >>= returnJson . taskJson taskId

-- | Update a task.
putTaskR :: TaskId -> Handler Value
putTaskR taskId = do
    body <- requireCheckJsonBody :: Handler Task
    case validateTask body of
        Left errors -> invalidArgs errors
        Right task -> update task
  where
    update =
        updateTask taskId
            >=> returnJson . taskJson taskId

-- | Delete a task.
deleteTaskR :: TaskId -> Handler ()
deleteTaskR =
    deleteTask

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
    lookupMilestone milestoneId
        >>= returnJson . milestoneJson milestoneId

putMilestoneR :: MilestoneId -> Handler Value
putMilestoneR milestoneId = do
    body <- requireCheckJsonBody :: Handler Milestone
    case validateMilestone body of
        Left errors -> invalidArgs errors
        Right milestone -> update milestone
  where
    update =
        updateMilestone milestoneId
            >=> returnJson . milestoneJson milestoneId

-- | Delete a milestone.
deleteMilestoneR :: MilestoneId -> Handler ()
deleteMilestoneR =
    deleteMilestone

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
deleteMilestoneTaskR =
    deleteMilestoneTask

-- Helper: run a page query and return as JSON.
queryPage :: (ToJSON a) => (Int -> Int -> Handler [a]) -> Handler Value
queryPage listQuery = do
    (PageParams pageSize pageNumber pageOffset) <- readPageParams
    pageData <- listQuery pageSize pageOffset
    returnJson $ pageJson pageSize pageNumber pageData
