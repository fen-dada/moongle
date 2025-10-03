{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Moongle.App.Runtime
  ( runCliApp
  , runAppWith
  , mkEnv
  ) where

import Control.Exception (bracket)
import Control.Lens ((^.))
import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple qualified as PSQL
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.FileSystem (runFileSystem)
import Effectful.Log
import Effectful.PostgreSQL qualified as EP
import Effectful.Reader.Static (runReader)
import Log.Backend.StandardOutput (withStdOutLogger)
import Moongle.App (runApp)
import Moongle.CLI (Cmd, Opts, parseArgsIO)
import Moongle.Config
import Moongle.Env (Env (Env))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Exit (exitFailure)

runCliApp :: IO ()
runCliApp = do
  (cmd, cliOpts) <- parseArgsIO
  runAppWith cmd cliOpts

runAppWith :: Cmd -> Opts -> IO ()
runAppWith cmd cliOpts = do
  econfig <- runEff $ runFileSystem $ runErrorNoCallStack $ genConfig cliOpts
  case econfig of
    Left err -> do
      putStrLn $ "Configuration error: " ++ err
      exitFailure
    Right config -> do
      env <- mkEnv config
      bracket (PSQL.connectPostgreSQL (toConnStr config)) PSQL.close $ \conn -> do
        result <-
          runEff $
            withStdOutLogger \stdoutLogger ->
              runErrorNoCallStack $
                runLog "moongle" stdoutLogger defaultLogLevel $
                  runReader config $
                    runReader env $
                      EP.runWithConnection conn $
                        runFileSystem $
                          runConcurrent $
                            runApp cmd
        case result of
          Left err -> putStrLn $ "Error: " <> err
          Right () -> pure ()

mkEnv :: Config -> IO Env
mkEnv _ = do
  utcNow <- getCurrentTime
  manager <- newManager tlsManagerSettings
  pure $ Env utcNow manager

-- | Build a libpq connection string from the Config.
toConnStr :: Config -> ByteString
toConnStr config =
  "host="
    <> enc (config ^. dbHost)
    <> " port="
    <> enc (T.pack (show (config ^. dbPort)))
    <> " dbname="
    <> enc (config ^. dbName)
    <> " user="
    <> enc (config ^. dbUser)
    <> " password="
    <> enc (config ^. dbPassword)
 where
  enc = encodeUtf8
