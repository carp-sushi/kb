module Database (
    createPool,
    runMigrations,
) where

import Model (migrateAll)
import Settings (Settings (..))

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (
    ConnectionPool,
    createPostgresqlPool,
    runMigrationSilent,
    runSqlPool,
 )

-- | Create a database connection pool.
createPool :: (MonadIO m) => Settings -> m ConnectionPool
createPool settings =
    if settingsVerboseLogging settings
        then mkPool runStdoutLoggingT
        else mkPool runNoLoggingT
  where
    mkPool loggingT =
        (liftIO . loggingT) $
            createPostgresqlPool
                (encodeUtf8 $ settingsDatabaseUrl settings)
                (settingsPoolSize settings)

-- | Run SQL migrations on a database.
runMigrations :: (MonadIO m) => Settings -> ConnectionPool -> m ()
runMigrations settings pool =
    if settingsVerboseLogging settings
        then runMigrations' runStdoutLoggingT
        else runMigrations' runNoLoggingT
  where
    runMigrations' loggingT =
        liftIO $ loggingT $ do
            _ <- runSqlPool (runMigrationSilent migrateAll) pool
            pure ()
