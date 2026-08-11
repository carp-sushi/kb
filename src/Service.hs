{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Service where

import Dto
import Foundation
import Model
import Page
import qualified Query

import Database.Persist.Sql
import Yesod.Core
import Yesod.Persist.Core (get404, runDB)

-- | List all boards as a JSON array.
listBoards :: Handler Value
listBoards = do
    (pageSize, pageNumber, pageOffset) <- readPageParams
    boards <- runDB $ selectList [] $ pageOpts pageSize pageOffset
    returnJson $ pageDto pageSize pageNumber boards
  where
    pageOpts limit_ offset_ =
        [ LimitTo limit_
        , OffsetBy offset_
        , Asc BoardPosition
        , Asc BoardName
        ]

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
    (pageSize, pageNumber, pageOffset) <- readPageParams
    tasks <- runDB $ selectList filterOpts $ pageOpts pageSize pageOffset
    returnJson $ pageDto pageSize pageNumber tasks
  where
    filterOpts = [TaskBoardId ==. boardId]
    pageOpts limit_ offset_ =
        [ LimitTo limit_
        , OffsetBy offset_
        , Asc TaskId
        ]

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

-- | List all milestones as a JSON array.
listMilestones :: Handler Value
listMilestones = do
    (pageSize, pageNumber, pageOffset) <- readPageParams
    milestones <- runDB $ selectList [] $ pageOpts pageSize pageOffset
    returnJson $ pageDto pageSize pageNumber milestones
  where
    pageOpts limit_ offset_ =
        [ LimitTo limit_
        , OffsetBy offset_
        , Desc MilestoneStartDate
        , Asc MilestoneCompleteDate
        ]

-- | Insert a milestone in the database and return it as a JSON value.
createMilestone :: Milestone -> Handler Value
createMilestone milestone = do
    inserted <- runDB $ insertEntity milestone
    returnJson inserted

-- | Lookup a milestone by id.
lookupMilestone :: MilestoneId -> Handler Value
lookupMilestone milestoneId = do
    milestone <- runDB $ get404 milestoneId
    returnJson $ milestoneDto milestoneId milestone

-- | Update a milestone in the database and return it as a JSON value.
updateMilestone :: MilestoneId -> Milestone -> Handler Value
updateMilestone milestoneId milestone = do
    updated <- runDB $ do
        update
            milestoneId
            [ MilestoneName =. milestoneName milestone
            , MilestoneStartDate =. milestoneStartDate milestone
            , MilestoneCompleteDate =. milestoneCompleteDate milestone
            ]
        get404 milestoneId
    returnJson $ milestoneDto milestoneId updated

-- | Delete a milestone.
deleteMilestone :: MilestoneId -> Handler ()
deleteMilestone milestoneId = runDB $ do
    _ <- get404 milestoneId
    delete milestoneId

-- | Link a milestone to a task.
createMilestoneTask :: MilestoneId -> TaskId -> Handler Value
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
    returnJson milestoneTask

-- | List all milestones linked to a task.
listTaskMilestones :: TaskId -> Handler Value
listTaskMilestones taskId = do
    (pageSize, pageNumber, pageOffset) <- readPageParams
    milestones <- runDB $ do
        _ <- get404 taskId
        Query.selectTaskMilestones taskId (fromIntegral pageSize) (fromIntegral pageOffset)
    returnJson $
        pageDto pageSize pageNumber milestones

-- | List all tasks linked to a milestone.
listMilestoneTasks :: MilestoneId -> Handler Value
listMilestoneTasks milestoneId = do
    (pageSize, pageNumber, pageOffset) <- readPageParams
    tasks <- runDB $ do
        _ <- get404 milestoneId
        Query.selectMilestoneTasks milestoneId (fromIntegral pageSize) (fromIntegral pageOffset)
    returnJson $
        pageDto pageSize pageNumber tasks

-- | Delete a link between a milestone and a task.
deleteMilestoneTask :: MilestoneId -> TaskId -> Handler ()
deleteMilestoneTask milestoneId taskId =
    runDB $ deleteWhere [MilestoneTaskMilestoneId ==. milestoneId, MilestoneTaskTaskId ==. taskId]
