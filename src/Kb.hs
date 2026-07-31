{-# LANGUAGE OverloadedStrings #-}

module Kb (runServer) where

import Application (runApp)
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
