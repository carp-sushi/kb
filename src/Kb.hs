{-# LANGUAGE OverloadedStrings #-}

module Kb (
    runServer,
    runMigrator,
) where

import Application (runApp, runMigrations)
import Data.Maybe (listToMaybe)
import Say (say)
import System.Environment (getArgs)

-- | Read settings file from command line and serve the application.
runServer :: IO ()
runServer = do
    args <- getArgs
    case listToMaybe args of
        Nothing -> say "Usage: kb-server <settings-file>"
        Just settingsFile -> runApp settingsFile

-- | Read settings file from command line and run database migrations.
runMigrator :: IO ()
runMigrator = do
    args <- getArgs
    case listToMaybe args of
        Nothing -> say "Usage: kb-migrator <settings-file>"
        Just settingsFile -> runMigrations settingsFile
