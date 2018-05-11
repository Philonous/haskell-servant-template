{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Run
    ( runMain
    ) where

import           Control.Concurrent.Async             (race)
import           Control.Concurrent.MVar
import           Control.Monad.Logger
import           Control.Monad.Trans
import qualified Data.Char                            as Char
import           Database.Persist.Sql                 ( runSqlPool
                                                      , runMigration
                                                      , runMigrationSilent
                                                      )
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger
import           System.Environment
import           System.Exit
import           System.IO
import qualified System.Posix.Signals                 as Posix

import           NejlaCommon                          (withPool)
import           Config                               (loadConf, getConfig
                                                      , getPort)
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

warpSettings :: Int -> Warp.Settings
warpSettings port =
    -- Give open connections 1 second to finish running
    Warp.setGracefulShutdownTimeout (Just 1)
  $ Warp.setPort port Warp.defaultSettings

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
    port <- getPort confFile
    withPool confFile 5 $ \pool -> do
      _ <-
        liftIO $
        runSqlPool
          (if logLevel <= LevelInfo
             then runMigration migrateAll
             else do
               _ <- runMigrationSilent migrateAll
               return ())
          pool
      -- Install handler for SIGTERM. Without this the server won't properly
      -- shut down e.g. on docker stop
      endVar <- liftIO $ newEmptyMVar
      _ <- liftIO $ Posix.installHandler
        Posix.sigTERM
        (Posix.Catch $ do
            hPutStrLn stderr "Received SIGTERM, stopping."
            putMVar endVar ()
        )
        Nothing
      _ <- liftIO $ race (takeMVar endVar) $
        Warp.runSettings
          (warpSettings port)
          (logMdw $ serveApp pool conf)
      return ()
