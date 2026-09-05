module Handler where

import Foundation
import Model
import Page
import Repo
import Validate

import Control.Monad ((>=>))
import Yesod.Core

-- GET /boards
getBoardsR :: Handler Value
getBoardsR =
    queryPage listBoards

-- POST /boards
postBoardsR :: Handler Value
postBoardsR = do
    body <- requireCheckJsonBody :: Handler Board
    case validateBoard body of
        Left errors -> invalidArgs errors
        Right board -> (createBoard >=> returnJson) board

-- GET /boards/#BoardId
getBoardR :: BoardId -> Handler Value
getBoardR boardId =
    fetchBoard boardId
        >>= returnJson

-- PUT /boards/#BoardId
putBoardR :: BoardId -> Handler Value
putBoardR boardId = do
    body <- requireCheckJsonBody :: Handler Board
    case validateBoard body of
        Left errors -> invalidArgs errors
        Right board -> (updateBoard boardId >=> returnJson) board

-- DELETE /boards/#BoardId
deleteBoardR :: BoardId -> Handler ()
deleteBoardR boardId =
    deleteBoard boardId
        >> sendResponseNoContent

-- GET /boards/#BoardId/tasks
getBoardTasksR :: BoardId -> Handler Value
getBoardTasksR boardId =
    queryPage $ listTasks boardId

-- POST /boards/#BoardId/tasks
postBoardTasksR :: BoardId -> Handler Value
postBoardTasksR boardId =
    (requireCheckJsonBody :: Handler TaskId)
        >>= moveTaskToBoard boardId
        >>= returnJson

-- POST /tasks
postTasksR :: Handler Value
postTasksR = do
    body <- requireCheckJsonBody :: Handler Task
    case validateTask body of
        Left errors -> invalidArgs errors
        Right task -> (createTask >=> returnJson) task

-- GET /tasks/#TaskId
getTaskR :: TaskId -> Handler Value
getTaskR taskId =
    fetchTask taskId
        >>= returnJson

-- PUT /tasks/#TaskId
putTaskR :: TaskId -> Handler Value
putTaskR taskId = do
    body <- requireCheckJsonBody :: Handler Task
    case validateTask body of
        Left errors -> invalidArgs errors
        Right task -> (updateTask taskId >=> returnJson) task

-- DELETE /tasks/#TaskId
deleteTaskR :: TaskId -> Handler ()
deleteTaskR taskId =
    deleteTask taskId
        >> sendResponseNoContent

-- GET /tasks/#TaskId/milestones
getTaskMilestonesR :: TaskId -> Handler Value
getTaskMilestonesR taskId =
    queryPage $ listTaskMilestones taskId

-- GET /milestones
getMilestonesR :: Handler Value
getMilestonesR =
    queryPage listMilestones

-- POST /milestones
postMilestonesR :: Handler Value
postMilestonesR = do
    body <- requireCheckJsonBody :: Handler Milestone
    case validateMilestone body of
        Left errors -> invalidArgs errors
        Right milestone -> (createMilestone >=> returnJson) milestone

-- GET /milestones/#MilestoneId
getMilestoneR :: MilestoneId -> Handler Value
getMilestoneR milestoneId =
    fetchMilestone milestoneId
        >>= returnJson

-- PUT /milestones/#MilestoneId
putMilestoneR :: MilestoneId -> Handler Value
putMilestoneR milestoneId = do
    body <- requireCheckJsonBody :: Handler Milestone
    case validateMilestone body of
        Left errors -> invalidArgs errors
        Right milestone -> (updateMilestone milestoneId >=> returnJson) milestone

-- DELETE /milestones/#MilestoneId
deleteMilestoneR :: MilestoneId -> Handler ()
deleteMilestoneR milestoneId =
    deleteMilestone milestoneId
        >> sendResponseNoContent

-- GET /milestones/#MilestoneId/tasks
getMilestoneTasksR :: MilestoneId -> Handler Value
getMilestoneTasksR milestoneId =
    queryPage $ listMilestoneTasks milestoneId

-- POST /milestones/#MilestoneId/tasks
postMilestoneTasksR :: MilestoneId -> Handler Value
postMilestoneTasksR milestoneId =
    (requireCheckJsonBody :: Handler TaskId)
        >>= createMilestoneTask milestoneId
        >>= returnJson

-- DELETE /milestones/#MilestoneId/tasks/#TaskId
deleteMilestoneTaskR :: MilestoneId -> TaskId -> Handler ()
deleteMilestoneTaskR milestoneId taskId =
    deleteMilestoneTask milestoneId taskId
        >> sendResponseNoContent
