{-# LANGUAGE OverloadedStrings #-}

module TaskSpec (spec) where

import Data.Aeson
import Data.Text (Text)
import Database.Persist.Sql (toSqlKey)
import TestSupport

spec :: Spec
spec = withApp $ do
    describe "list tasks for a board" $ do
        it "returns 200" $ do
            boardId <- runDB $ insert $ Board "Test Board" Red 1
            request $ do
                setMethod "GET"
                setUrl $ BoardTasksR boardId
                addRequestHeader ("Accept", "application/json")
            statusIs 200

    describe "get task" $ do
        it "returns 200 when a task exists" $ do
            taskId <- runDB $ do
                bid <- insert $ Board "Test Board" Red 1
                tid <- insert $ Task bid "Test Task" 1 Todo
                pure tid
            request $ do
                setMethod "GET"
                setUrl $ TaskR taskId
                addRequestHeader ("Accept", "application/json")
            statusIs 200

        it "returns 404 when a task does not exist" $ do
            request $ do
                setMethod "GET"
                setUrl $ TaskR (toSqlKey 0)
                addRequestHeader ("Accept", "application/json")
            statusIs 404

    describe "create task" $ do
        it "returns 200 when JSON body is valid and task validation passes" $ do
            boardId <- runDB $ insert $ Board "Test Board" Red 1
            request $ do
                setMethod "POST"
                setUrl $ TasksR
                addRequestHeader ("Content-Type", "application/json")
                setRequestBody $
                    encode $
                        object
                            [ "boardId" .= boardId
                            , "name" .= ("Test Task" :: Text)
                            , "points" .= (1 :: Int)
                            , "status" .= Todo
                            ]
            statusIs 200

        it "returns 400 when JSON body is invalid" $ do
            request $ do
                setMethod "POST"
                setUrl $ TasksR
                addRequestHeader ("Content-Type", "application/json")
                setRequestBody $ encode $ object ["foo" .= ("Test Task" :: Value)]
            statusIs 400

        it "returns 400 when JSON body is valid but task validation fails" $ do
            boardId <- runDB $ insert $ Board "Test Board" Red 1
            request $ do
                setMethod "POST"
                setUrl $ TasksR
                addRequestHeader ("Content-Type", "application/json")
                setRequestBody $
                    encode $
                        object
                            [ "boardId" .= boardId
                            , "name" .= ("" :: Text) -- invalid
                            , "points" .= (0 :: Int) -- invalid
                            , "status" .= Todo
                            ]
            statusIs 400

    describe "update task" $ do
        it "returns 200 when JSON body is valid and task validation passes" $ do
            (boardId, taskId) <- runDB $ do
                bid <- insert $ Board "Test Board" Red 1
                tid <- insert $ Task bid "Test Task" 1 Todo
                pure (bid, tid)
            request $ do
                setMethod "PUT"
                setUrl $ TaskR taskId
                addRequestHeader ("Content-Type", "application/json")
                setRequestBody $
                    encode $
                        object
                            [ "boardId" .= boardId
                            , "name" .= ("Updated Task" :: Text)
                            , "points" .= (2 :: Int)
                            , "status" .= Done
                            ]
            statusIs 200

        it "returns 400 when JSON body is invalid" $ do
            taskId <- runDB $ do
                bid <- insert $ Board "Test Board" Red 1
                tid <- insert $ Task bid "Test Task" 1 Todo
                pure tid
            request $ do
                setMethod "PUT"
                setUrl $ TaskR taskId
                addRequestHeader ("Content-Type", "application/json")
                setRequestBody $ encode $ object ["foo" .= ("Test Task" :: Value)]
            statusIs 400

        it "returns 400 when JSON body is valid but task validation fails" $ do
            (boardId, taskId) <- runDB $ do
                bid <- insert $ Board "Test Board" Red 1
                tid <- insert $ Task bid "Test Task" 1 Todo
                pure (bid, tid)
            request $ do
                setMethod "PUT"
                setUrl $ TaskR taskId
                addRequestHeader ("Content-Type", "application/json")
                setRequestBody $
                    encode $
                        object
                            [ "boardId" .= boardId
                            , "name" .= ("" :: Text) -- invalid
                            , "points" .= (0 :: Int) -- invalid
                            , "status" .= Done
                            ]
            statusIs 400

    describe "delete task" $ do
        it "returns 200 when a task is deleted" $ do
            taskId <- runDB $ do
                bid <- insert $ Board "Test Board" Red 1
                tid <- insert $ Task bid "Test Task" 1 Todo
                pure tid
            request $ do
                setMethod "DELETE"
                setUrl $ TaskR taskId
            statusIs 200

        it "returns 404 when a task does not exist" $ do
            request $ do
                setMethod "DELETE"
                setUrl $ TaskR (toSqlKey 0)
            statusIs 404
