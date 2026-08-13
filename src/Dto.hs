{-# LANGUAGE OverloadedStrings #-}

module Dto (
    boardJson,
    milestoneJson,
    pageJson,
    taskJson,
) where

import Data.Aeson
import Model

-- | Create a JSON data transfer object for a board.
boardJson :: BoardId -> Board -> Value
boardJson boardId (Board name color position) =
    object
        [ "id" .= boardId
        , "name" .= name
        , "color" .= color
        , "position" .= position
        ]

-- | Create a JSON data transfer object for a milestone.
milestoneJson :: MilestoneId -> Milestone -> Value
milestoneJson milestoneId (Milestone name startDate completeDate) =
    object
        [ "id" .= milestoneId
        , "name" .= name
        , "startDate" .= startDate
        , "completeDate" .= completeDate
        ]

-- | Create a JSON data transfer object for a page.
pageJson :: (ToJSON a) => Int -> Int -> [a] -> Value
pageJson pageSize pageNumber pageData =
    object
        [ "pageSize" .= pageSize
        , "previousPageNumber" .= max 1 (pageNumber - 1)
        , "pageNumber" .= pageNumber
        , "nextPageNumber" .= (pageNumber + 1)
        , "pageData" .= pageData
        ]

-- | Create a JSON data transfer object for a task.
taskJson :: TaskId -> Task -> Value
taskJson taskId (Task boardId name points status) =
    object
        [ "id" .= taskId
        , "boardId" .= boardId
        , "name" .= name
        , "points" .= points
        , "status" .= status
        ]
