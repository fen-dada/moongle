{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Moongle.Config where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Aeson
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.YAML.Aeson
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString.Lazy
import GHC.Generics (Generic)
import Moongle.CLI qualified as CLI
import System.FilePath
import Prelude hiding (readFile)

data Config = Config
  { _registryUrl :: Text
  , _mooncakesBaseUrl :: Text
  , _moongleStoragePath :: Text
  , _parallel :: Int
  , _dbHost :: Text
  , _dbPort :: Int
  , _dbName :: Text
  , _dbUser :: Text
  , _dbPassword :: Text
  }
  deriving (Show, Generic)

makeLenses ''Config

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}

instance Default Config where
  def =
    Config
      { _registryUrl = "https://mooncakes.io/assets/modules.json"
      , _moongleStoragePath = ""
      , _mooncakesBaseUrl = "https://moonbitlang-mooncakes.s3.us-west-2.amazonaws.com/user"
      , _parallel = 32
      , _dbHost = "localhost"
      , _dbPort = 5432
      , _dbName = "postgres"
      , _dbUser = "postgres"
      , _dbPassword = ""
      }

data PartialConfig = PartialConfig
  { _registryUrlP :: Maybe Text
  , _mooncakesBaseUrlP :: Maybe Text
  , _moongleStoragePathP :: Maybe Text
  , _parallelP :: Maybe Int
  , _dbHostP :: Maybe Text
  , _dbPortP :: Maybe Int
  , _dbNameP :: Maybe Text
  , _dbUserP :: Maybe Text
  , _dbPasswordP :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON PartialConfig where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1 . init}

makeLenses ''PartialConfig

instance Semigroup PartialConfig where
  a <> b =
    PartialConfig
      { _registryUrlP = a ^. registryUrlP <|> b ^. registryUrlP
      , _mooncakesBaseUrlP = a ^. mooncakesBaseUrlP <|> b ^. mooncakesBaseUrlP
      , _moongleStoragePathP = a ^. moongleStoragePathP <|> b ^. moongleStoragePathP
      , _parallelP = a ^. parallelP <|> b ^. parallelP
      , _dbHostP = a ^. dbHostP <|> b ^. dbHostP
      , _dbPortP = a ^. dbPortP <|> b ^. dbPortP
      , _dbNameP = a ^. dbNameP <|> b ^. dbNameP
      , _dbUserP = a ^. dbUserP <|> b ^. dbUserP
      , _dbPasswordP = a ^. dbPasswordP <|> b ^. dbPasswordP
      }

instance Monoid PartialConfig where
  mempty =
    PartialConfig
      { _registryUrlP = Nothing
      , _mooncakesBaseUrlP = Nothing
      , _moongleStoragePathP = Nothing
      , _parallelP = Nothing
      , _dbHostP = Nothing
      , _dbPortP = Nothing
      , _dbNameP = Nothing
      , _dbUserP = Nothing
      , _dbPasswordP = Nothing
      }

applyPartial :: Config -> PartialConfig -> Config
applyPartial base partial =
  base
    & registryUrl .~ fromMaybe (base ^. registryUrl) (partial ^. registryUrlP)
    & mooncakesBaseUrl .~ fromMaybe (base ^. mooncakesBaseUrl) (partial ^. mooncakesBaseUrlP)
    & parallel .~ fromMaybe (base ^. parallel) (partial ^. parallelP)
    & dbHost .~ fromMaybe (base ^. dbHost) (partial ^. dbHostP)
    & dbPort .~ fromMaybe (base ^. dbPort) (partial ^. dbPortP)
    & dbName .~ fromMaybe (base ^. dbName) (partial ^. dbNameP)
    & dbUser .~ fromMaybe (base ^. dbUser) (partial ^. dbUserP)
    & dbPassword .~ fromMaybe (base ^. dbPassword) (partial ^. dbPasswordP)

optsToPartial :: CLI.Opts -> PartialConfig
optsToPartial (CLI.Opts _cfg host name port user password) =
  PartialConfig
    { _registryUrlP = Nothing
    , _dbHostP = host
    , _dbNameP = name
    , _dbPortP = port
    , _dbUserP = user
    , _dbPasswordP = password
    , _mooncakesBaseUrlP = Nothing
    , _moongleStoragePathP = Nothing
    , _parallelP = Nothing
    }

-- | Build a Config from CLI Opts (pure defaults + $HOME/.moongle).
genConfig :: (FileSystem :> es, Error String :> es) => CLI.Opts -> Eff es Config
genConfig opts = do
  partialFromFile <- loadPartialConfigFromFile (CLI.configPath opts)
  let partialFromOpts = optsToPartial opts
  let finalPartial = partialFromOpts <> partialFromFile
  let baseConfig = applyPartial def finalPartial
  homeDir <- getHomeDirectory
  let storagePath = T.pack $ homeDir </> ".moongle"
  pure $ baseConfig & moongleStoragePath .~ storagePath

loadPartialConfigFromFile :: (FileSystem :> es, Error String :> es) => Maybe FilePath -> Eff es PartialConfig
loadPartialConfigFromFile mPath = do
  defConfDir <- getXdgDirectory XdgConfig "moongle"
  let path = fromMaybe (defConfDir </> "config.yaml") mPath

  exists <- doesFileExist path
  if exists
    then do
      content <- readFile path
      either (throwError_ . show) pure (decode1 content)
    else do
      pure mempty
