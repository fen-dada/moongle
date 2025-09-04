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
import Log.Backend.StandardOutput
import Moongle.CLI
import Moongle.Config
import Moongle.DB
import Moongle.Env
import Moongle.Query
import Moongle.Query.Parser (text2Query)
import Moongle.Registry
import Moongle.Server (server)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
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
      env <- mkEnv config
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
                    runReader env $
                      EP.runWithConnection conn $
                        runFileSystem $
                          runConcurrent $
                            app cmd

          case result of
            Left err -> putStrLn $ "Error: " <> err
            Right _ -> pure ()

appUpdate :: (Reader Env :> es, Error String :> es, Log :> es, Reader Config :> es, FileSystem :> es, Concurrent :> es, IOE :> es, EP.WithConnection :> es) => UpdateOpts -> Eff es ()
appUpdate _o = do
  runMigrations
  fetchAllPackages
  mbtis <- parseAllMbtiWithPkg
  _s <- insertMbtiToDB mbtis
  pure ()

appServe :: (Reader Env :> es, Reader Config :> es, Error String :> es, Log :> es, IOE :> es, EP.WithConnection :> es) => ServeOpts -> Eff es ()
appServe _o = do
  server

appSearch :: (Error String :> es, Log :> es, Reader Config :> es, Reader Env :> es, IOE :> es, EP.WithConnection :> es) => SearchOpts -> Eff es ()
appSearch (SearchOpts str l) = do
  q <- either (\e -> throwError ("Failed to parse query: " ++ show e)) pure (text2Query str)
  result <- queryDefSummary q
  liftIO $ print (take l result)

app :: (Reader Env :> es, Error String :> es, Log :> es, Reader Config :> es, FileSystem :> es, Concurrent :> es, IOE :> es, EP.WithConnection :> es) => Cmd -> Eff es ()
app = \case
  Update o -> appUpdate o
  Serve o -> appServe o
  Search o -> appSearch o

mkEnv :: config -> IO Env
mkEnv _c = do
  utcNow <- getCurrentTime
  manager <- newManager tlsManagerSettings
  pure $ Env utcNow manager
