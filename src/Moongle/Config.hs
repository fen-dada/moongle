{-# LANGUAGE TemplateHaskell #-}
module Moongle.Config where

import Data.Text qualified as T
import GHC.Generics (Generic)
import Control.Lens

data Config = Config
  {_registryUrl :: T.Text, _mooncakesBaseUrl :: T.Text, _moongleStoragePath :: T.Text}
  deriving (Show, Generic)

makeLenses ''Config
