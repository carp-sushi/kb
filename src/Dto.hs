{-# LANGUAGE OverloadedStrings #-}

module Dto (
    boardDto,
    milestoneDto,
    taskDto,
) where

import Data.Aeson
import Model

-- | Create a JSON data transfer object for a board.
boardDto :: BoardId -> Board -> Value
boardDto boardId (Board name color position) =
    object
        [ "id" .= boardId
        , "name" .= name
        , "color" .= color
        , "position" .= position
        ]

-- | Create a JSON data transfer object for a milestone.
milestoneDto :: MilestoneId -> Milestone -> Value
milestoneDto milestoneId (Milestone name startDate completeDate) =
    object
        [ "id" .= milestoneId
        , "name" .= name
        , "startDate" .= startDate
        , "completeDate" .= completeDate
        ]

-- | Create a JSON data transfer object for a task.
taskDto :: TaskId -> Task -> Value
taskDto taskId (Task boardId name points status) =
    object
        [ "id" .= taskId
        , "boardId" .= boardId
        , "name" .= name
        , "points" .= points
        , "status" .= status
        ]
