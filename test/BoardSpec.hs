{-# LANGUAGE OverloadedStrings #-}

module BoardSpec (spec) where

import Data.Aeson
import Data.Text (Text)
import Database.Persist.Sql (toSqlKey)
import TestSupport

spec :: Spec
spec = withApp $ do
    describe "list boards" $ do
        it "returns 200" $ do
            request $ do
                setMethod "GET"
                setUrl BoardsR
                addRequestHeader ("Accept", "application/json")
            statusIs 200

    describe "get board" $ do
        it "returns 200 when a board exists" $ do
            boardId <- runDB $ insert $ Board "Test Board" Red 1
            request $ do
                setMethod "GET"
                setUrl $ BoardR boardId
                addRequestHeader ("Accept", "application/json")
            statusIs 200

        it "returns 404 when a board does not exist" $ do
            request $ do
                setMethod "GET"
                setUrl $ BoardR (toSqlKey 0)
                addRequestHeader ("Accept", "application/json")
            statusIs 404

    describe "create board" $ do
        it "returns 200 when JSON body is valid and board validation passes" $ do
            request $ do
                setMethod "POST"
                setUrl BoardsR
                addRequestHeader ("Content-Type", "application/json")
                setRequestBody $
                    encode $
                        object
                            [ "name" .= ("Test Board" :: Text)
                            , "color" .= Green
                            , "position" .= (1 :: Int)
                            ]
            statusIs 200

        it "returns 400 when JSON body is invalid" $ do
            request $ do
                setMethod "POST"
                setUrl BoardsR
                addRequestHeader ("Content-Type", "application/json")
                setRequestBody $ encode $ object ["foo" .= ("Test Board" :: Value)]
            statusIs 400

        it "returns 400 when JSON body is valid but board validation fails" $ do
            request $ do
                setMethod "POST"
                setUrl BoardsR
                addRequestHeader ("Content-Type", "application/json")
                setRequestBody $
                    encode $
                        object
                            [ "name" .= (" " :: Text)  -- invalid
                            , "color" .= Green
                            , "position" .= (0 :: Int) -- invalid
                            ]
            statusIs 400

    describe "update board" $ do
        it "returns 200 when JSON body is valid and board validation passes" $ do
            boardId <- runDB $ insert $ Board "Test Board" Red 1
            request $ do
                setMethod "PUT"
                setUrl $ BoardR boardId
                addRequestHeader ("Content-Type", "application/json")
                setRequestBody $
                    encode $
                        object
                            [ "name" .= ("Updated Board" :: Text)
                            , "color" .= Yellow
                            , "position" .= (2 :: Int)
                            ]
            statusIs 200

        it "returns 400 when JSON body is invalid" $ do
            boardId <- runDB $ insert $ Board "Test Board" Red 1
            let body = object ["foo" .= ("Test Board" :: Value)]
            request $ do
                setMethod "PUT"
                setUrl $ BoardR boardId
                addRequestHeader ("Content-Type", "application/json")
                setRequestBody $ encode body
            statusIs 400

        it "returns 400 when JSON body is valid but board validation fails" $ do
            boardId <- runDB $ insert $ Board "Test Board" Red 1
            request $ do
                setMethod "PUT"
                setUrl $ BoardR boardId
                addRequestHeader ("Content-Type", "application/json")
                setRequestBody $
                    encode $
                        object
                            [ "name" .= (" " :: Text)  -- invalid
                            , "color" .= Yellow
                            , "position" .= (0 :: Int) -- invalid
                            ]
            statusIs 400

    describe "delete board" $ do
        it "returns 200 when an existing board is deleted" $ do
            boardId <- runDB $ insert $ Board "Test Board" Red 1
            request $ do
                setMethod "DELETE"
                setUrl $ BoardR boardId
                addRequestHeader ("Content-Type", "application/json")
            statusIs 200

        it "returns 404 when a board does not exist" $ do
            request $ do
                setMethod "DELETE"
                setUrl $ BoardR (toSqlKey 0)
                addRequestHeader ("Content-Type", "application/json")
            statusIs 404
