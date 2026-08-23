{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Settings (
    Settings (..),
    loadSettings,
    settingsReadHttpPort,
) where

import Data.Configurator
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Text.Read (readMaybe)

-- | App settings type.
data Settings = Settings
    { settingsDatabaseUrl :: Text
    , settingsPoolSize :: Int
    , settingsHttpPort :: Text
    , settingsVerboseLogging :: Bool
    , settingsEnvironment :: Text
    }
    deriving (Eq, Ord, Show)

-- | Load settings from file.
loadSettings :: FilePath -> IO Settings
loadSettings filePath = do
    cfg <- load [Required filePath]
    settingsDatabaseUrl <- require cfg "databaseUrl"
    settingsPoolSize <- require cfg "poolSize"
    settingsHttpPort <- require cfg "httpPort"
    settingsVerboseLogging <- require cfg "verboseLogging"
    settingsEnvironment <- require cfg "environment"
    pure Settings{..}

-- | Read HTTP port from settings or return a default (3000).
settingsReadHttpPort :: Settings -> Int
settingsReadHttpPort =
    fromMaybe 3000 . readMaybe . unpack . settingsHttpPort
