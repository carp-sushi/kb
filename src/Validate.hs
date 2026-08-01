{-# LANGUAGE OverloadedStrings #-}

module Validate (
    validateBoard,
    validateTask,
) where

import qualified Data.Text as T
import Data.Validation
import Model

-- Validation errors
data Error
    = NameTooLong
    | NameIsEmpty
    | InvalidPosition
    | InvalidPoints

-- Validation error text adapter
errorText :: Error -> T.Text
errorText NameIsEmpty = "name must be non-empty"
errorText NameTooLong = "name must be at most 500 characters"
errorText InvalidPosition = "position must be >= 0"
errorText InvalidPoints = "points must be between 1 and 8"

-- | Validate a board
validateBoard :: Board -> Either [T.Text] Board
validateBoard board =
    case validateBoard' board of
        (Success board') -> Right board'
        (Failure errors) -> Left $ fmap errorText errors

-- Internal board validation
validateBoard' :: Board -> Validation [Error] Board
validateBoard' (Board name color position) =
    mkBoard
        <$> validateName name
        <*> validatePosition position
  where
    mkBoard n = Board n color

-- | Validate a task
validateTask :: Task -> Either [T.Text] Task
validateTask task =
    case validateTask' task of
        (Success task') -> Right task'
        (Failure errors) -> Left $ fmap errorText errors

-- Internal task validation
validateTask' :: Task -> Validation [Error] Task
validateTask' (Task boardId name points status) =
    mkTask
        <$> validateName name
        <*> validatePoints points
  where
    mkTask n p = Task boardId n p status

-- Internal name validation
validateName :: String -> Validation [Error] String
validateName name
    | T.null t = Failure [NameIsEmpty]
    | T.length t > 500 = Failure [NameTooLong]
    | otherwise = Success $ T.unpack t
  where
    t = (T.strip . T.pack) name

-- Internal position validation
validatePosition :: Int -> Validation [Error] Int
validatePosition position =
    if position < 0
        then Failure [InvalidPosition]
        else Success position

-- Internal points validation
validatePoints :: Int -> Validation [Error] Int
validatePoints points =
    if points < 1 || points > 8
        then Failure [InvalidPoints]
        else Success points
