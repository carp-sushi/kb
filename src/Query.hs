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
    Int ->
    Int ->
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
            (toInt64 limitTo)
        offset
            (toInt64 offsetBy)
        pure m

-- | Select tasks linked to a milestone.
selectMilestoneTasks ::
    (MonadIO m) =>
    MilestoneId ->
    Int ->
    Int ->
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
            (toInt64 limitTo)
        offset
            (toInt64 offsetBy)
        pure t

-- TODO: This is listed as a dangerous function.
--       Find a better way to do this.
toInt64 :: Int -> Int64
toInt64 = fromIntegral
