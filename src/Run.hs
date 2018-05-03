{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Run
    ( runMain
    ) where

import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Char                            as Char
import           Database.Persist.Sql                 ( runSqlPool
                                                      , runMigration
                                                      , runMigrationSilent
                                                      )
import           Network.Wai
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           System.Environment
import           System.Exit
import           System.IO

import           NejlaCommon                          (withPool)

import           Api                                  (api)
import           App                                  (run)
import           Config                               (loadConf, getConfig)
import           Handlers
import           Persist.Schema                       (migrateAll)


parseLogLevel :: String -> Maybe LogLevel
parseLogLevel str =
  case map (Char.toLower) str of
    "debug" -> Just LevelDebug
    "info"  -> Just LevelInfo
    "warn"  -> Just LevelWarn
    "error" -> Just LevelError
    _ -> Nothing

runMain :: IO ()
runMain = do
  mbLogLevel <- lookupEnv "log"
  (logMdw, logLevel) <-
    case mbLogLevel of
      Nothing -> return (logStdout, LevelWarn)
      Just str ->
        case parseLogLevel str of
          Just LevelDebug -> return (logStdoutDev, LevelDebug)
          Just lvl -> return (logStdout, lvl)
          Nothing -> do
            hPutStrLn stderr $ "Error: Could not parse log level " ++ str
            exitFailure
  runStderrLoggingT . filterLogger (\_source level -> level >= logLevel) $ do
    confFile <- loadConf "auth_service"
    conf <- getConfig confFile
    withPool confFile 5 $ \pool -> do
      _ <-
        liftIO $
        runSqlPool
          (if logLevel <= LevelInfo
             then runMigration migrateAll
             else do
              _ <- runMigrationSilent migrateAll
              return ()
          )
          pool
      liftIO $ Warp.run 80 (logMdw $ serveApp pool conf)
