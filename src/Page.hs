{-# LANGUAGE OverloadedStrings #-}

module Page (
    PageParams (..),
    queryPage,
    readPageParams,
) where

import Data.Text (Text, unpack)
import Foundation
import Text.Read (readMaybe)
import Yesod.Core

-- | Parameters for querying a page of data.
data PageParams
    = PageParams !Int !Int
    deriving
        (Eq, Ord, Show)

-- | Read page parameters from request query params.
readPageParams :: Handler PageParams
readPageParams = do
    PageParams
        <$> readPageSize
        <*> readPageNumber
  where
    readPageSize = parsePageSize <$> lookupGetParam "pageSize"
    readPageNumber = parsePageNumber <$> lookupGetParam "pageNumber"

-- Parse page size and clamp it within a set range (1-100, default 10).
parsePageSize :: Maybe Text -> Int
parsePageSize = clamp . parseInt
  where
    clamp Nothing = 10
    clamp (Just n) = max 1 $ min 100 n

-- Parse page number and ensure it is a positive integer (default 1).
parsePageNumber :: Maybe Text -> Int
parsePageNumber = clamp . parseInt
  where
    clamp Nothing = 1
    clamp (Just n) = max 1 n

-- Convert text to int if defined.
parseInt :: Maybe Text -> Maybe Int
parseInt =
    (>>= readMaybe . unpack)

-- | Run a page query and return as JSON.
queryPage :: (ToJSON a) => (Int -> Int -> Handler [a]) -> Handler Value
queryPage pageQuery =
    readPageParams
        >>= executeQuery pageQuery
        >>= returnPageJson

-- Execute a list query, returning a page (params and data).
executeQuery :: (Int -> Int -> Handler [a]) -> PageParams -> Handler (PageParams, [a])
executeQuery pageQuery pageParams@(PageParams size number) =
    pageQuery size (size * (number - 1))
        >>= \pageData -> pure (pageParams, pageData)

-- Render a JSON data transfer object for a page.
returnPageJson :: (ToJSON a) => (PageParams, [a]) -> Handler Value
returnPageJson (PageParams size number, pageData) =
    returnJson $
        object
            [ "pageSize" .= size
            , "previousPageNumber" .= max 1 (number - 1)
            , "pageNumber" .= number
            , "nextPageNumber" .= (number + 1)
            , "pageData" .= pageData
            ]
