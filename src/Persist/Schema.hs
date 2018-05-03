{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Persist.Schema
  ( module Persist.Schema
  ) where

import Control.Lens
import Data.Text              (Text)
import Data.Time.Clock
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH

import NejlaCommon

share [ mkPersist sqlSettings, mkMigrate "migrateAll"]
      $(persistFileWith lowerCaseSettings "src/Persist/schema")

makeLensesWith camelCaseFields ''User
