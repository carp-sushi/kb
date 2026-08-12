{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Query(
    findMilestoneTask,
    selectTaskMilestones,
    selectMilestoneTasks,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Int (Int64)
import Database.Esqueleto.Experimental
import Model

-- | Find a link between a milestone and a task if there is one.
findMilestoneTask ::
    (MonadIO m) =>
    MilestoneId ->
    TaskId ->
    SqlPersistT m (Maybe (Entity MilestoneTask))
findMilestoneTask milestoneId taskId =
    selectOne $ do
        ms <- from $ table @MilestoneTask
        where_ $
            ms ^. MilestoneTaskMilestoneId ==. val milestoneId &&.
            ms ^. MilestoneTaskTaskId ==. val taskId
        pure ms

-- | Select milestones linked to a task.
selectTaskMilestones ::
    (MonadIO m) =>
    TaskId ->
    Int64 ->
    Int64 ->
    SqlPersistT m [Entity Milestone]
selectTaskMilestones taskId limitTo offsetBy =
    select $ do
        (m :& mt) <- from $
            table @Milestone
            `innerJoin`
            table @MilestoneTask
            `on` \(m :& mt) -> m ^. MilestoneId ==. mt ^. MilestoneTaskMilestoneId
        where_ $
            mt ^. MilestoneTaskTaskId ==. val taskId
        orderBy
            [desc $ m ^. MilestoneStartDate]
        limit
            limitTo
        offset
            offsetBy
        pure m

-- | Select tasks linked to a milestone.
selectMilestoneTasks ::
    (MonadIO m) =>
    MilestoneId ->
    Int64 ->
    Int64 ->
    SqlPersistT m [Entity Task]
selectMilestoneTasks milestoneId limitTo offsetBy =
    select $ do
        (s :& mt) <- from $
            table @Task
            `innerJoin`
            table @MilestoneTask
            `on` \(s :& mt) -> s ^. TaskId ==. mt ^. MilestoneTaskTaskId
        where_ $
            mt ^. MilestoneTaskMilestoneId ==. val milestoneId
        orderBy
            [asc $ s ^. TaskId]
        limit
            limitTo
        offset
            offsetBy
        pure s
