-- TODO: Rewrite using optparse-generic
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Moongle.CLI (
  Cmd (..),
  UpdateOpts (..),
  ServeOpts (..),
  Opts (..),
  SearchOpts (..),
  args,
  parseArgsIO,
)
where

import Data.Text (Text)
import Effectful
import GHC.Generics
import Options.Applicative

data Cmd = Update UpdateOpts | Serve ServeOpts | Search SearchOpts
  deriving (Show, Generic)

data UpdateOpts = UpdateOpts {url :: Text, jobs :: Int}
  deriving (Show, Generic)

data ServeOpts = ServeOpts {host :: Maybe Text, port :: Maybe Int}
  deriving (Show, Generic)

data Opts = Opts {configPath :: Maybe FilePath, dbHost :: Maybe Text, dbName :: Maybe Text, dbPort :: Maybe Int, dbUser :: Maybe Text, dbPassword :: Maybe Text}
  deriving (Show, Generic)

data SearchOpts = SearchOpts
  { query :: Text
  , limit :: Int
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
    <$> optional (strOption (long "host" <> short 'h' <> metavar "HOST" <> help "Host to bind"))
    <*> optional (option auto (long "port" <> short 'p' <> metavar "PORT" <> help "Port to bind"))

pSearchOpts :: Parser SearchOpts
pSearchOpts =
  SearchOpts
    <$> strOption (long "query" <> short 'q' <> metavar "QUERY" <> help "Search query")
    <*> option auto (long "limit" <> short 'l' <> metavar "LIMIT" <> help "Number of results to return")

pOpts :: Parser Opts
pOpts =
  Opts
    <$> optional (strOption (long "config" <> short 'c' <> metavar "CONFIG" <> help "Path to config file"))
    <*> optional (strOption (long "db_host" <> metavar "DB_HOST" <> help "Database host"))
    <*> optional (strOption (long "db_name" <> metavar "DB_NAME" <> help "Database name"))
    <*> optional (option auto (long "db_port" <> metavar "DB_PORT" <> help "Database port"))
    <*> optional (strOption (long "db_user" <> metavar "DB_USER" <> help "Database user"))
    <*> optional (strOption (long "db_password" <> metavar "DB_PASSWORD" <> help "Database password"))

pArgs :: ParserInfo (Cmd, Opts)
pArgs = info ((,) <$> pCmd <*> pOpts <**> helper) (header "Moongle - Moonbit API search engine")

parseArgsIO :: IO (Cmd, Opts)
parseArgsIO = execParser pArgs

args :: (IOE :> es) => Eff es (Cmd, Opts)
args = liftIO parseArgsIO
