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

-- | Find a link between a milestone and a task if one exists.
findMilestoneTask ::
    (MonadIO m) =>
    MilestoneId ->
    TaskId ->
    SqlPersistT m (Maybe (Entity MilestoneTask))
findMilestoneTask milestoneId taskId =
    selectOne $ do
        mt <- from $ table @MilestoneTask
        where_ $
            mt ^. MilestoneTaskMilestoneId ==. val milestoneId &&.
            mt ^. MilestoneTaskTaskId ==. val taskId
        pure mt

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
        (t :& mt) <- from $
            table @Task
            `innerJoin`
            table @MilestoneTask
            `on` \(t :& mt) -> t ^. TaskId ==. mt ^. MilestoneTaskTaskId
        where_ $
            mt ^. MilestoneTaskMilestoneId ==. val milestoneId
        orderBy
            [asc $ t ^. TaskId]
        limit
            limitTo
        offset
            offsetBy
        pure t
