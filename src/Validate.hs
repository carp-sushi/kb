{-# LANGUAGE OverloadedStrings #-}

module Validate (
    validateBoard,
    validateMilestone,
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
validateBoard =
    mkEither validateBoard'

-- Internal board validation
validateBoard' :: Board -> Validation [Error] Board
validateBoard' (Board name color position) =
    Board
        <$> validateName name
        <*> Success color
        <*> validatePosition position
  where
    validatePosition = check (< 0) InvalidPosition

-- | Validate a milestone
validateMilestone :: Milestone -> Either [T.Text] Milestone
validateMilestone =
    mkEither validateMilestone'

-- Internal milestone validation
validateMilestone' :: Milestone -> Validation [Error] Milestone
validateMilestone' (Milestone name startDate completeDate) =
    mkMilestone
        <$> validateName name
  where
    mkMilestone n = Milestone n startDate completeDate

-- | Validate a task
validateTask :: Task -> Either [T.Text] Task
validateTask =
    mkEither validateTask'

-- Internal task validation
validateTask' :: Task -> Validation [Error] Task
validateTask' (Task boardId name points status) =
    Task
        <$> Success boardId
        <*> validateName name
        <*> validatePoints points
        <*> Success status
  where
    validatePoints = check (\p -> p < 1 || p > 8) InvalidPoints

-- Call a validation function and transform the result to an Either type.
mkEither :: (a -> Validation [Error] b) -> a -> Either [T.Text] b
mkEither validate' a =
    case validate' a of
        Success b -> Right b
        Failure errors -> Left $ fmap errorText errors

-- Internal name validation
validateName :: T.Text -> Validation [Error] T.Text
validateName name
    | T.null t = Failure [NameIsEmpty]
    | T.length t > 500 = Failure [NameTooLong]
    | otherwise = Success t
  where
    t = T.strip name

-- Validation helper using a failure predicate function.
check :: (a -> Bool) -> Error -> a -> Validation [Error] a
check isFailure e a =
    if isFailure a
        then Failure [e]
        else Success a
