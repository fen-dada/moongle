{-# LANGUAGE TemplateHaskell #-}

module Moongle.Registry.Types where

import Control.Lens
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Servant
import Servant.Client

data ModuleInfo = ModuleInfo {_name :: Text, _version :: Text, _description :: Maybe Text}
  deriving (Show, Generic)

instance FromJSON ModuleInfo where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}

makeLenses ''ModuleInfo

newtype Registry = Registry {_modules :: V.Vector ModuleInfo}
  deriving (Show, Generic)

instance FromJSON Registry where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}

makeLenses ''Registry

-- See docs/NOTES.md
type MooncakesApi =
  "user"
    :> Capture "username" Text -- <username>
    :> Capture "modulename" Text -- <modulename>
    :> Capture "version" Text -- eg. 0.1.0.zip
    :> Get '[OctetStream] BL.ByteString

-- See docs/NOTES.md
type RegistryApi = "assets" :> "modules.json" :> Get '[JSON] Registry

queryMooncake :: Text -> Text -> Text -> ClientM BL.ByteString
queryMooncake = client mooncackesApi
 where
  mooncackesApi :: Proxy MooncakesApi
  mooncackesApi = Proxy

queryRegistry :: ClientM Registry
queryRegistry = client regApi
 where
  regApi :: Proxy RegistryApi
  regApi = Proxy
