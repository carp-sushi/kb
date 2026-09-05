{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Repo where

import Foundation
import Model
import qualified Query

import Data.Foldable (traverse_)
import Database.Persist.Sql
import Yesod.Core
import Yesod.Persist.Core (get404, runDB)

-- | List a page of boards ordered by position and name.
listBoards :: Int -> Int -> Handler [Entity Board]
listBoards limitTo offsetBy =
    runDB $
        selectList
            []
            [ LimitTo limitTo
            , OffsetBy offsetBy
            , Asc BoardPosition
            , Asc BoardName
            ]

-- | Lookup a board by id.
lookupBoard :: BoardId -> Handler (Entity Board)
lookupBoard boardId =
    mkEntity boardId
        <$> runDB (get404 boardId)

-- | Insert a board in the database and return the inserted entity.
createBoard :: Board -> Handler (Entity Board)
createBoard board =
    runDB $ insertEntity board

-- | Update a board in the database and return the updated record.
updateBoard :: BoardId -> Board -> Handler (Entity Board)
updateBoard boardId board = do
    updated <- runDB $ do
        update
            boardId
            [ BoardName =. boardName board
            , BoardColor =. boardColor board
            , BoardPosition =. boardPosition board
            ]
        get404 boardId
    pure $ mkEntity boardId updated

-- | Delete a board, all tasks on the board, and any milestone links to those tasks.
deleteBoard :: BoardId -> Handler ()
deleteBoard boardId =
    runDB $ do
        _ <- get404 boardId
        tasks <- selectList [TaskBoardId ==. boardId] []
        traverse_ (\(Entity taskId _) -> deleteWhere [MilestoneTaskTaskId ==. taskId]) tasks
        deleteWhere [TaskBoardId ==. boardId]
        delete boardId

-- | List a page of tasks on a board.
listTasks :: BoardId -> Int -> Int -> Handler [Entity Task]
listTasks boardId limitTo offsetBy =
    runDB $
        selectList
            [TaskBoardId ==. boardId]
            [ LimitTo limitTo
            , OffsetBy offsetBy
            , Desc TaskPoints
            , Asc TaskName
            ]

-- | Lookup a task by id.
lookupTask :: TaskId -> Handler (Entity Task)
lookupTask taskId =
    mkEntity taskId
        <$> runDB (get404 taskId)

-- | Insert a task in the database and return the inserted entity.
createTask :: Task -> Handler (Entity Task)
createTask task =
    runDB $ do
        _ <- get404 $ taskBoardId task
        insertEntity task

-- | Update a task in the database and return the updated record.
updateTask :: TaskId -> Task -> Handler (Entity Task)
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
    pure $ mkEntity taskId updated

-- | Move task to another board.
moveTaskToBoard :: BoardId -> TaskId -> Handler (Entity Task)
moveTaskToBoard boardId taskId = do
    task <- runDB $ do
        _ <- get404 boardId
        update taskId [TaskBoardId =. boardId]
        get404 taskId
    pure $ mkEntity taskId task

-- | Delete a task and any links to milestones.
deleteTask :: TaskId -> Handler ()
deleteTask taskId =
    runDB $ do
        _ <- get404 taskId
        deleteWhere [MilestoneTaskTaskId ==. taskId]
        delete taskId

-- | List a page of milestones ordered by start date descending.
listMilestones :: Int -> Int -> Handler [Entity Milestone]
listMilestones limitTo offsetBy =
    runDB $
        selectList
            []
            [ LimitTo limitTo
            , OffsetBy offsetBy
            , Desc MilestoneStartDate
            , Asc MilestoneCompleteDate
            ]

-- | Insert a milestone in the database and return the inserted entity.
createMilestone :: Milestone -> Handler (Entity Milestone)
createMilestone milestone =
    runDB $ insertEntity milestone

-- | Lookup a milestone by id.
lookupMilestone :: MilestoneId -> Handler (Entity Milestone)
lookupMilestone milestoneId =
    mkEntity milestoneId
        <$> runDB (get404 milestoneId)

-- | Update a milestone in the database and return the updated record.
updateMilestone :: MilestoneId -> Milestone -> Handler (Entity Milestone)
updateMilestone milestoneId milestone = do
    updated <- runDB $ do
        update
            milestoneId
            [ MilestoneName =. milestoneName milestone
            , MilestoneStartDate =. milestoneStartDate milestone
            , MilestoneCompleteDate =. milestoneCompleteDate milestone
            ]
        get404 milestoneId
    pure $ mkEntity milestoneId updated

-- | Delete a milestone and any links to tasks.
deleteMilestone :: MilestoneId -> Handler ()
deleteMilestone milestoneId =
    runDB $ do
        _ <- get404 milestoneId
        deleteWhere [MilestoneTaskMilestoneId ==. milestoneId]
        delete milestoneId

-- | Link a milestone to a task.
createMilestoneTask :: MilestoneId -> TaskId -> Handler MilestoneTask
createMilestoneTask milestoneId taskId = do
    (Entity _ milestoneTask) <- runDB $ do
        maybeEntity <- Query.findMilestoneTask milestoneId taskId
        case maybeEntity of
            Just entity -> do
                $logWarn "Milestone already linked to task"
                pure entity
            Nothing -> do
                _ <- get404 milestoneId
                _ <- get404 taskId
                insertEntity $ MilestoneTask milestoneId taskId
    pure milestoneTask

-- | List a page of milestones linked to a task.
listTaskMilestones :: TaskId -> Int -> Int -> Handler [Entity Milestone]
listTaskMilestones taskId limitTo offsetBy =
    runDB $
        Query.selectTaskMilestones
            taskId
            limitTo
            offsetBy

-- | List a page of tasks linked to a milestone.
listMilestoneTasks :: MilestoneId -> Int -> Int -> Handler [Entity Task]
listMilestoneTasks milestoneId limitTo offsetBy =
    runDB $
        Query.selectMilestoneTasks
            milestoneId
            limitTo
            offsetBy

-- | Delete a link between a milestone and a task.
deleteMilestoneTask :: MilestoneId -> TaskId -> Handler ()
deleteMilestoneTask milestoneId taskId =
    runDB $ do
        _ <- get404 milestoneId
        _ <- get404 taskId
        deleteWhere
            [ MilestoneTaskMilestoneId ==. milestoneId
            , MilestoneTaskTaskId ==. taskId
            ]

-- Helper for constructiong an entity from a key and record.
mkEntity :: Key record -> record -> Entity record
mkEntity entityKey entityVal =
    Entity{..}
