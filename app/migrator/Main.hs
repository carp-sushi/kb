module Main (main) where

import Kb (runMigrator)

-- Run the database migrations
main :: IO ()
main = runMigrator
