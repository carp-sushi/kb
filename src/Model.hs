{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import Data.Aeson
import Database.Persist.Quasi
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics

-- | Custom field type for board color.
data BoardColor
    = Red
    | Yellow
    | Blue
    | Green
    | Magenta
    | Cyan
    | White
    deriving
        (Eq, Generic, Ord, Read, Show)

-- Define persistence and JSON encoding / decoding for the board color type.
derivePersistField "BoardColor"
instance ToJSON BoardColor
instance FromJSON BoardColor

-- | Custom field type for task status.
data TaskStatus
    = Todo
    | Doing
    | Done
    deriving
        (Eq, Generic, Ord, Read, Show)

-- Define persistence and JSON encoding / decoding for the task status type.
derivePersistField "TaskStatus"
instance ToJSON TaskStatus
instance FromJSON TaskStatus

-- | Define database models and migrations.
share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
