{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Moongle.App (main) where

import Control.Exception
import Control.Lens
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple qualified as PSQL
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.Log
import Effectful.PostgreSQL as EP
import Effectful.Reader.Static
import Effectful.Wreq
import Log.Backend.StandardOutput
import Moongle.CLI
import Moongle.Config
import Moongle.DB
import Moongle.Env
import Moongle.Query
import Moongle.Query.Parser (text2Query)
import Moongle.Registry hiding (opts)
import Moongle.Server (server)
import System.Exit

main :: IO ()
main = do
  -- parse CLI once
  (cmd, cliOpts) <- runEff args

  -- build Config (needs FileSystem to get $HOME)
  econfig <- runEff $ runFileSystem $ runErrorNoCallStack $ genConfig cliOpts
  case econfig of
    Left err -> do
      putStrLn $ "Configuration error: " ++ err
      exitFailure
    Right config -> do
      -- connect to Postgres
      bracket
        ( PSQL.connectPostgreSQL
            ( "host="
                <> encodeUtf8 (config ^. Moongle.Config.dbHost)
                <> " port="
                <> encodeUtf8 (T.pack (show (config ^. Moongle.Config.dbPort)))
                <> " dbname="
                <> encodeUtf8 (config ^. Moongle.Config.dbName)
                <> " user="
                <> encodeUtf8 (config ^. Moongle.Config.dbUser)
                <> " password="
                <> encodeUtf8 (config ^. Moongle.Config.dbPassword)
            )
        )
        PSQL.close
        \conn -> do
          -- run the effect stack and dispatch on the command
          result <- runEff $
            withStdOutLogger \stdoutLogger ->
              runErrorNoCallStack $
                runLog "moongle" stdoutLogger defaultLogLevel $
                  runReader config $
                    runWreq $
                      EP.runWithConnection conn $
                        runFileSystem $
                          runConcurrent $
                            app cmd

          case result of
            Left err -> putStrLn $ "Error: " <> err
            Right _ -> pure ()

appUpdate :: (Error String :> es, Log :> es, Reader Config :> es, FileSystem :> es, Concurrent :> es, Wreq :> es, IOE :> es, EP.WithConnection :> es) => UpdateOpts -> Eff es ()
appUpdate _o = do
  runMigrations
  fetchAllPackages
  mbtis <- parseAllMbtiWithPkg
  _s <- insertMbtiToDB mbtis
  pure ()

appServe :: (Error String :> es, Log :> es, IOE :> es, EP.WithConnection :> es) => ServeOpts -> Eff es ()
appServe _o = do
  utcNow <- liftIO getCurrentTime
  runReader (Env utcNow) server

appSearch :: (Error String :> es, Log :> es, Reader Config :> es, IOE :> es, EP.WithConnection :> es) => SearchOpts -> Eff es ()
appSearch (SearchOpts str l) = do
  q <- either (\e -> throwError ("Failed to parse query: " ++ show e)) pure (text2Query str)
  result <- queryDefSummary q
  liftIO $ print (take l result)

app :: (Error String :> es, Log :> es, Reader Config :> es, FileSystem :> es, Concurrent :> es, Wreq :> es, IOE :> es, EP.WithConnection :> es) => Cmd -> Eff es ()
app = \case
  Update o -> appUpdate o
  Serve o -> appServe o
  Search o -> appSearch o
