{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}

module Config
  ( module NejlaCommon.Config
  , getPort
  , getConfig
  ) where

import           Control.Lens            ((^.))
import           Control.Monad.Logger    (LoggingT)
import           Control.Monad.Trans
import qualified Data.Aeson              as Aeson
import qualified Data.Configurator.Types as Conf
import           Data.Default            (def)
import           Data.Monoid
import qualified Data.Text               as Text
--------------------------------------------------------------------------------
--  ----------------------------------------------------------------------------
--------------------------------------------------------------------------------

import           NejlaCommon.Config      hiding (Config)

import qualified App

--------------------------------------------------------------------------------
-- Configuration ---------------------------------------------------------------
--------------------------------------------------------------------------------

getPort :: Conf.Config -> LoggingT IO Int
getPort conf =
  getConf' "PORT" "http.port" (Right 80) conf

-- | Parse config information
getConfig :: Conf.Config -> LoggingT IO App.Config
getConfig conf = return App.Config
