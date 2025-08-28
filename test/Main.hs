{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Control.Lens
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Database.PostgreSQL.Simple qualified as PSQL
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.Log
import Effectful.PostgreSQL qualified as EP
import Effectful.Reader.Static
import Effectful.Wreq
import Log.Backend.StandardOutput
import Moongle.Config
import Moongle.DB (runMigrations)
import Moongle.Env
import Moongle.Query
import Moongle.Registry
import Moongle.Server
import System.FilePath

makeTestConfig :: (FileSystem :> es) => Eff es Config
makeTestConfig = do
  homeDir <- getHomeDirectory
  let storagePath = homeDir </> ".moongle"
  pure
    Config
      { _registryUrl = "https://mooncakes.io/assets/modules.json",
        _moongleStoragePath = T.pack storagePath,
        _mooncakesBaseUrl = "https://moonbitlang-mooncakes.s3.us-west-2.amazonaws.com/user",
        _parallel = 32,
        _dbHost = "localhost",
        _dbPort = 5432,
        _dbName = "postgres",
        _dbUser = "postgres",
        _dbPassword = ""
      }

serverTest :: (Error String :> es, Log :> es, Reader Config :> es, FileSystem :> es, Concurrent :> es, Wreq :> es, IOE :> es, EP.WithConnection :> es) => Eff es ()
serverTest = do
  fetchAllPackages
  mbtis <- parseAllMbtiWithPkg
  _s <- insertMbtiToDB mbtis
  utcNow <- liftIO getCurrentTime
  runReader (Env utcNow) server

runTest :: Eff '[Concurrent, FileSystem, EP.WithConnection, Wreq, Reader Config, Log, Error String, IOE] () -> IO ()
runTest action = do
  testConfig <- runEff $ runFileSystem makeTestConfig
  a <- bracket (PSQL.connectPostgreSQL ("host=" <> encodeUtf8 (testConfig ^. dbHost) <> " dbname=" <> encodeUtf8 (testConfig ^. dbName))) PSQL.close $ \conn -> runEff $ withStdOutLogger $ \stdoutLogger ->
    runErrorNoCallStack $ runLog "demo" stdoutLogger defaultLogLevel $ runReader testConfig $ runWreq $ EP.runWithConnection conn $ runFileSystem $ runConcurrent action
  case a of
    Left err -> putStrLn $ "Error: " ++ err
    Right _ -> putStrLn "Test completed successfully"

main :: IO ()
main = do
  runTest runMigrations
  runTest serverTest
