{-# LANGUAGE OverloadedStrings #-}

module Page (
    PageSize (..),
    PageNumber (..),
    PageParams (..),
    queryPage,
    readPageParams,
) where

import Data.Text (Text, unpack)
import Foundation
import Text.Read (readMaybe)
import Yesod.Core

-- | A type for page size
newtype PageSize = PageSize Int
    deriving (Eq, Ord, Show)

-- | A type for page number
newtype PageNumber = PageNumber Int
    deriving (Eq, Ord, Show)

-- | Parameters for querying a page of data.
data PageParams = PageParams !PageSize !PageNumber
    deriving (Eq, Ord, Show)

-- | A type alias for a page.
type Page a = (PageSize, PageNumber, [a])

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
parsePageSize :: Maybe Text -> PageSize
parsePageSize = PageSize . clamp . parseInt
  where
    clamp Nothing = 10
    clamp (Just n) = max 1 $ min 100 n

-- Parse page number and ensure it is a positive integer (default 1).
parsePageNumber :: Maybe Text -> PageNumber
parsePageNumber = PageNumber . clamp . parseInt
  where
    clamp Nothing = 1
    clamp (Just n) = max 1 n

-- Convert text to int if defined.
parseInt :: Maybe Text -> Maybe Int
parseInt =
    (>>= readMaybe . unpack)

-- | Run a page query and return as JSON.
queryPage :: (ToJSON a) => (Int -> Int -> Handler [a]) -> Handler Value
queryPage listQuery =
    readPageParams
        >>= executeQuery listQuery
        >>= returnPageJson

-- Execute a list query, returning a page (params and data).
executeQuery :: (Int -> Int -> Handler [a]) -> PageParams -> Handler (Page a)
executeQuery listQuery (PageParams pageSize pageNumber) =
    listQuery size (size * (number - 1))
        >>= \pageData -> pure (pageSize, pageNumber, pageData)
  where
    (PageSize size) = pageSize
    (PageNumber number) = pageNumber

-- Render a JSON data transfer object for a page.
returnPageJson :: (ToJSON a) => Page a -> Handler Value
returnPageJson (PageSize size, PageNumber number, pageData) =
    returnJson $
        object
            [ "pageSize" .= size
            , "previousPageNumber" .= max 1 (number - 1)
            , "pageNumber" .= number
            , "nextPageNumber" .= (number + 1)
            , "pageData" .= pageData
            ]
