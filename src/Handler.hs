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
    either
        invalidArgs
        (createBoard >=> returnJson)
        (validateBoard body)

-- | Get a board.
getBoardR :: BoardId -> Handler Value
getBoardR boardId =
    lookupBoard boardId
        >>= returnJson . boardDto boardId

-- | Update a board.
putBoardR :: BoardId -> Handler Value
putBoardR boardId = do
    body <- requireCheckJsonBody :: Handler Board
    either
        invalidArgs
        (updateBoard boardId >=> (returnJson . boardDto boardId))
        (validateBoard body)

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
    either
        invalidArgs
        (createTask >=> returnJson)
        (validateTask body)

-- | Get a task.
getTaskR :: TaskId -> Handler Value
getTaskR taskId =
    lookupTask taskId
        >>= returnJson . taskDto taskId

-- | Update a task.
putTaskR :: TaskId -> Handler Value
putTaskR taskId = do
    body <- requireCheckJsonBody :: Handler Task
    either
        invalidArgs
        (updateTask taskId >=> (returnJson . taskDto taskId))
        (validateTask body)

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
    either
        invalidArgs
        (createMilestone >=> returnJson)
        (validateMilestone body)

-- | Get a milestone.
getMilestoneR :: MilestoneId -> Handler Value
getMilestoneR milestoneId =
    lookupMilestone milestoneId
        >>= returnJson . milestoneDto milestoneId

putMilestoneR :: MilestoneId -> Handler Value
putMilestoneR milestoneId = do
    body <- requireCheckJsonBody :: Handler Milestone
    either
        invalidArgs
        (updateMilestone milestoneId >=> (returnJson . milestoneDto milestoneId))
        (validateMilestone body)

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
    (pageSize, pageNumber, pageOffset) <- readPageParams
    pageData <- listQuery pageSize pageOffset
    returnJson $ pageDto pageSize pageNumber pageData
