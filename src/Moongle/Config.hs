{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Moongle.Config where

import Control.Lens
import Data.Text qualified as T
import GHC.Generics (Generic)

data Config = Config
  { _registryUrl :: T.Text,
    _mooncakesBaseUrl :: T.Text,
    _moongleStoragePath :: T.Text,
    _parallel :: Int,
    _dbHost :: T.Text,
    _dbPort :: Int,
    _dbName :: T.Text,
    _dbUser :: T.Text,
    _dbPassword :: T.Text
  }
  deriving (Show, Generic)

makeLenses ''Config
