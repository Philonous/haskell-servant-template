{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module App
  (module App
  , Sql.ConnectionPool
  )
where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Default
import qualified Database.Persist.Sql as Sql
import qualified NejlaCommon          as NC

data Config = Config
  {
  } deriving (Show)

makeLensesWith camelCaseFields ''Config

newtype AppState = AppState
  { appStateConfig :: Config
  }

makeLensesWith camelCaseFields ''AppState

type App a = NC.App AppState 'NC.Privileged 'NC.ReadCommitted a

run :: MonadIO m => Sql.ConnectionPool -> Config -> App a -> m a
run pool conf m =
  let st = AppState { appStateConfig = conf }
  in liftIO $ NC.runApp' def pool st m

db :: ReaderT Sql.SqlBackend IO a -> App a
db = NC.db'

getConfig ::  Lens' Config a -> App a
getConfig g = NC.viewState $ config . g
