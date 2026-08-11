{-# LANGUAGE BlockArguments #-}

module Handler where

import Foundation
import Model
import Service
import Validate

import Yesod.Core

-- | List all boards ordered by position.
getBoardsR :: Handler Value
getBoardsR = listBoards

-- | Create a board.
postBoardsR :: Handler Value
postBoardsR = do
    body <- requireCheckJsonBody :: Handler Board
    either invalidArgs createBoard (validateBoard body)

-- | Get a board.
getBoardR :: BoardId -> Handler Value
getBoardR = lookupBoard

-- | Update a board.
putBoardR :: BoardId -> Handler Value
putBoardR boardId = do
    body <- requireCheckJsonBody :: Handler Board
    either invalidArgs (updateBoard boardId) (validateBoard body)

-- | Delete a board and all tasks on the board.
deleteBoardR :: BoardId -> Handler ()
deleteBoardR = deleteBoard

-- | Get all tasks on a board.
getBoardTasksR :: BoardId -> Handler Value
getBoardTasksR = listTasks

-- | Create a task.
postTasksR :: Handler Value
postTasksR = do
    body <- requireCheckJsonBody :: Handler Task
    either invalidArgs createTask (validateTask body)

-- | Get a task.
getTaskR :: TaskId -> Handler Value
getTaskR = lookupTask

-- | Update a task.
putTaskR :: TaskId -> Handler Value
putTaskR taskId = do
    body <- requireCheckJsonBody :: Handler Task
    either invalidArgs (updateTask taskId) (validateTask body)

-- | Delete a task.
deleteTaskR :: TaskId -> Handler ()
deleteTaskR = deleteTask

-- | Get all milestones linked to a task.
getTaskMilestonesR :: TaskId -> Handler Value
getTaskMilestonesR = listTaskMilestones

-- | List all milestones.
getMilestonesR :: Handler Value
getMilestonesR = listMilestones

-- | Create a milestone.
postMilestonesR :: Handler Value
postMilestonesR = do
    body <- requireCheckJsonBody :: Handler Milestone
    either invalidArgs createMilestone (validateMilestone body)

-- | Get a milestone.
getMilestoneR :: MilestoneId -> Handler Value
getMilestoneR = lookupMilestone

putMilestoneR :: MilestoneId -> Handler Value
putMilestoneR milestoneId = do
    body <- requireCheckJsonBody :: Handler Milestone
    either invalidArgs (updateMilestone milestoneId) (validateMilestone body)

-- | Delete a milestone.
deleteMilestoneR :: MilestoneId -> Handler ()
deleteMilestoneR = deleteMilestone

-- | Get all tasks linked to a milestone.
getMilestoneTasksR :: MilestoneId -> Handler Value
getMilestoneTasksR = listMilestoneTasks

-- | Link a milestone to a task
postMilestoneTasksR :: MilestoneId -> Handler Value
postMilestoneTasksR milestoneId = do
    taskId <- requireCheckJsonBody :: Handler TaskId
    createMilestoneTask milestoneId taskId

-- | Unlink a milestone from a task
deleteMilestoneTaskR :: MilestoneId -> TaskId -> Handler ()
deleteMilestoneTaskR = deleteMilestoneTask
