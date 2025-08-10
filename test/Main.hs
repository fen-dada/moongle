{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Data.Text qualified as T
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
import Moongle.Env
import Moongle.Query
import Moongle.Registry
import Moongle.Server
import System.FilePath

runMigrations :: (EP.WithConnection :> es, IOE :> es, Log :> es) => Eff es ()
runMigrations = EP.withTransaction $ do
  logInfo_ "Running migrations…"

  _ <- EP.execute_ "DROP TABLE IF EXISTS defs"
  _ <- EP.execute_
    "CREATE TABLE IF NOT EXISTS defs ( \
    \ def_id bigserial PRIMARY KEY, \
    \ username text NOT NULL, \
    \ mod    text NOT NULL, \
    \ pkg_path text[] NOT NULL, \
    \ pkg_version text NOT NULL, \
    \ fun_name    text NOT NULL, \
    \ pretty_sig  text NOT NULL, \
    \ visibility  text NOT NULL, \
    \ kind        text NOT NULL, \
    \ tokens_lex  text[] NOT NULL, \
    \ arity int NOT NULL, \
    \ has_async boolean NOT NULL, \
    \ may_raise boolean NOT NULL, \
    \ version_tag text NOT NULL, \
    \ src_file text, src_line int, src_col int \
    \ )"

  _ <- EP.execute_ "DROP FUNCTION IF EXISTS my_array_to_string(ANYARRAY, TEXT);"
  _ <- EP.execute_ "CREATE FUNCTION my_array_to_string(arr ANYARRAY, sep TEXT) RETURNS text LANGUAGE SQL IMMUTABLE AS 'SELECT array_to_string(arr, sep)';"
  _ <- EP.execute_ "ALTER TABLE defs DROP COLUMN IF EXISTS tokens"
  _ <- EP.execute_
    "ALTER TABLE defs \
    \  ADD COLUMN tokens tsvector GENERATED ALWAYS AS \
    \    (to_tsvector('simple'::regconfig, my_array_to_string(tokens_lex,' '))) STORED"

  -- 3) gin index on tokens
  _ <- EP.execute_ "CREATE INDEX IF NOT EXISTS defs_tokens_gin ON defs USING gin (tokens)"
  _ <- EP.execute_ "CREATE INDEX IF NOT EXISTS defs_pkg ON defs (username, mod, pkg_version)"

  logInfo_ "Migrations done."
  pure ()

makeTestConfig :: (FileSystem :> es) => Eff es Config
makeTestConfig = do
  homeDir <- getHomeDirectory
  let storagePath = homeDir </> ".moongle"
  pure
    Config
      { _registryUrl = "https://mooncakes.io/assets/modules.json",
        _moongleStoragePath = T.pack storagePath,
        _mooncakesBaseUrl = "https://moonbitlang-mooncakes.s3.us-west-2.amazonaws.com/user",
        _parallel = 32
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
  a <- bracket (PSQL.connectPostgreSQL "host=localhost dbname=postgres") PSQL.close $ \conn -> runEff $ withStdOutLogger $ \stdoutLogger ->
    runErrorNoCallStack $ runLog "demo" stdoutLogger defaultLogLevel $ runReader testConfig $ runWreq $ EP.runWithConnection conn $ runFileSystem $ runConcurrent action
  case a of
    Left err -> putStrLn $ "Error: " ++ err
    Right _ -> putStrLn "Test completed successfully"

main :: IO ()
main = do
  runTest runMigrations
  runTest serverTest
