{-# LANGUAGE TemplateHaskell #-}

module Moongle.Registry.Types where

import Control.Lens
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Servant

data ModuleInfo = ModuleInfo {_name :: T.Text, _version :: T.Text, _description :: Maybe T.Text}
  deriving (Show, Generic)

instance FromJSON ModuleInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

makeLenses ''ModuleInfo

newtype Registry = Registry {_modules :: V.Vector ModuleInfo}
  deriving (Show, Generic)

instance FromJSON Registry where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

makeLenses ''Registry

-- See docs/NOTES.md
type MooncakesApi =
  "user"
    :> Capture "username" T.Text -- <username>
    :> Capture "modulename" T.Text -- <modulename>
    :> Capture "version" T.Text -- eg. 0.1.0.zip
    :> Get '[OctetStream] BL.ByteString

-- See docs/NOTES.md
type RegistryApi = "assets" :> "modules.json" :> Get '[JSON] Registry
