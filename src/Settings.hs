{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Settings (
    Settings (..),
    loadSettings,
    settingsReadHttpPort,
) where

import Data.Configurator
import Data.Text (Text, unpack)
import Text.Read (readMaybe)

-- | App settings type.
data Settings = Settings
    { settingsDatabaseUrl :: Text
    , settingsPoolSize :: Int
    , settingsHttpPort :: Text
    , settingsRunMigrations :: Bool
    , settingsVerboseLogging :: Bool
    }
    deriving (Eq, Ord, Show)

-- | Load settings from file.
loadSettings :: FilePath -> IO Settings
loadSettings filePath = do
    cfg <- load [Required filePath]
    settingsDatabaseUrl <- require cfg "databaseUrl"
    settingsPoolSize <- require cfg "poolSize"
    settingsHttpPort <- require cfg "httpPort"
    settingsRunMigrations <- require cfg "runMigrations"
    settingsVerboseLogging <- require cfg "verboseLogging"
    pure Settings{..}

-- | Read HTTP port from settings or return a default (3000).
settingsReadHttpPort :: Settings -> Int
settingsReadHttpPort settings =
    case maybeReadHttpPort settings of
        (Just port) -> port
        Nothing -> 3000

-- Helper: Read HTTP port from settings if possible.
maybeReadHttpPort :: Settings -> Maybe Int
maybeReadHttpPort =
    readMaybe . unpack . settingsHttpPort
