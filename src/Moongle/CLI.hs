{-# LANGUAGE DuplicateRecordFields #-}

module Moongle.CLI
  ( args,
    Cmd (..),
    UpdateOpts (..),
    ServeOpts (..),
    Opts (..),
    SearchOpts (..),
  )
where

import Effectful
import GHC.Generics
import Options.Applicative

data Cmd = Update UpdateOpts | Serve ServeOpts | Search SearchOpts
  deriving (Show, Generic)

data UpdateOpts = UpdateOpts {url :: String, jobs :: Int}
  deriving (Show, Generic)

data ServeOpts = ServeOpts {host :: String, port :: Int}
  deriving (Show, Generic)

data Opts = Opts {configPath :: FilePath, dbHost :: String, dbName :: String}
  deriving (Show, Generic)

data SearchOpts = SearchOpts
  { query :: String,
    limit :: Int
  }
  deriving (Show, Generic)

pCmd :: Parser Cmd
pCmd =
  hsubparser $
    command "update" (info (Update <$> pUpdateOpts) (progDesc "Update local database from the registry"))
      <> command "serve" (info (Serve <$> pServeOpts) (progDesc "Run the HTTP server"))
      <> command "search" (info (Search <$> pSearchOpts) (progDesc "Search definitions"))

pUpdateOpts :: Parser UpdateOpts
pUpdateOpts =
  UpdateOpts
    <$> strOption (long "url" <> short 'u' <> metavar "URL" <> help "Registry URL" <> value "https://moonbitlang-mooncakes.s3.us-west-2.amazonaws.com/user" <> showDefault)
    <*> option auto (long "jobs" <> short 'j' <> metavar "JOBS" <> help "Parallel jobs" <> value 32 <> showDefault)

pServeOpts :: Parser ServeOpts
pServeOpts =
  ServeOpts
    <$> strOption (long "host" <> short 'h' <> metavar "HOST" <> help "Host to bind" <> value "0.0.0.0" <> showDefault)
    <*> option auto (long "port" <> short 'p' <> metavar "PORT" <> help "Port to bind" <> value 8080 <> showDefault)

pSearchOpts :: Parser SearchOpts
pSearchOpts =
  SearchOpts
    <$> strOption (long "query" <> short 'q' <> metavar "QUERY" <> help "Search query" <> showDefault)
    <*> option auto (long "limit" <> short 'l' <> metavar "LIMIT" <> help "Number of results to return" <> value 20 <> showDefault)

pOpts :: Parser Opts
pOpts =
  Opts
    <$> strOption (long "config" <> short 'c' <> metavar "CONFIG" <> help "Path to config file" <> value "config.yaml" <> showDefault)
    <*> strOption (long "db_host" <> metavar "DB_HOST" <> help "Database host" <> value "localhost" <> showDefault)
    <*> strOption (long "db_name" <> metavar "DB_NAME" <> help "Database name" <> value "postgres" <> showDefault)

pArgs :: ParserInfo (Cmd, Opts)
pArgs = info ((,) <$> pCmd <*> pOpts <**> helper) (header "Moongle - Moonbit API search engine")

args :: (IOE :> es) => Eff es (Cmd, Opts)
args = liftIO $ execParser pArgs
