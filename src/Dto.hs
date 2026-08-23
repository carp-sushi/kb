{-# LANGUAGE OverloadedStrings #-}

module Dto (pageJson) where

import Data.Aeson

-- | Create a JSON data transfer object for a page.
pageJson :: (ToJSON a) => Int -> Int -> [a] -> Value
pageJson pageSize pageNumber pageData =
    object
        [ "pageSize" .= pageSize
        , "previousPageNumber" .= max 1 (pageNumber - 1)
        , "pageNumber" .= pageNumber
        , "nextPageNumber" .= (pageNumber + 1)
        , "pageData" .= pageData
        ]
