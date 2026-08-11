{-# LANGUAGE OverloadedStrings #-}

module MilestoneTaskSpec (spec) where

import TestSupport
import Data.Aeson
import Data.Time.Clock (getCurrentTime)

spec :: Spec
spec = withApp $ do
    describe "link milestone to task" $ do
        it "returns 200 when request is valid" $ do
            startDate <- liftIO $ getCurrentTime
            (milestoneId, taskId) <- runDB $ do
                mid <- insert $ Milestone "Milestone" (Just startDate) Nothing
                bid <- insert $ Board "Board" Green 2
                tid <- insert $ Task bid "Task" 4 Doing
                pure (mid, tid)
            request $ do
                setMethod "POST"
                setUrl $ MilestoneTasksR milestoneId
                setRequestBody $ encode taskId
                addRequestHeader ("Content-Type", "application/json")
            statusIs 200

    describe "delete milestone task link" $ do
        it "returns 200 when a link exists" $ do
            (milestoneId, taskId) <- runDB $ do
                mid <- insert $ Milestone "Milestone" Nothing Nothing
                bid <- insert $ Board "Board" Green 2
                tid <- insert $ Task bid "Task" 4 Doing
                _ <- insert $ MilestoneTask mid tid
                pure (mid, tid)
            request $ do
                setMethod "DELETE"
                setUrl $ MilestoneTaskR milestoneId taskId
                addRequestHeader ("Accept", "application/json")
            statusIs 200
