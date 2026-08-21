{-# LANGUAGE OverloadedStrings #-}

module Page (
    PageParams (..),
    readPageParams,
) where

import Data.Text (Text, unpack)
import Foundation
import Text.Read (readMaybe)
import Yesod.Core

-- | Parameters for querying a page of data.
data PageParams
    = PageParams !Int !Int !Int
    deriving
        (Eq, Ord, Show)

-- | Read page parameters from request query params.
readPageParams :: Handler PageParams
readPageParams =
    (mkPageParams . parsePageSize <$> lookupGetParam "pageSize")
        <*> (parsePageNumber <$> lookupGetParam "pageNumber")
  where
    mkPageParams s n =
        PageParams s n (s * (n - 1))

-- Parse page size and clamp it within a set range (1-100, default 10).
parsePageSize :: Maybe Text -> Int
parsePageSize =
    maybe 10 (max 1 . min 100) . parseInt

-- Parse page number and ensure it is a positive integer (default 1).
parsePageNumber :: Maybe Text -> Int
parsePageNumber =
    maybe 1 (max 1) . parseInt

-- Convert text to int if defined.
parseInt :: Maybe Text -> Maybe Int
parseInt =
    (>>= readMaybe . unpack)
