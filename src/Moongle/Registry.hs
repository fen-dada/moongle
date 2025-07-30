{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Moongle.Registry where

import Prelude hiding (writeFile)
import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Lens
import Data.Aeson (FromJSON)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString
import Effectful.Reader.Static
import Effectful.Wreq
import GHC.Generics (Generic)
import Moongle.Config
import System.FilePath

data ModuleInfo = ModuleInfo {name :: T.Text, version :: T.Text, description :: T.Text}
  deriving (Show, Generic)

instance FromJSON ModuleInfo

newtype Registry = Registry {modules :: V.Vector ModuleInfo}
  deriving (Show, Generic)

instance FromJSON Registry

fetchRegistry :: (Wreq :> es) => Eff es Registry
fetchRegistry = do
  resp <- get "https://mooncakes.io/assets/modules.json"
  reg <- asJSON resp
  pure $ reg ^. responseBody

downloadModule :: (Wreq :> es, FileSystem :> es, Reader Config :> es, IOE :> es) => (T.Text, T.Text, T.Text) -> Eff es ()
downloadModule (uname, pname, ver) = do
  lp <- T.unpack <$> asks (^. moongleStoragePath)
  baseurl <- asks (^. mooncakesBaseUrl)
  let url = T.unpack $ baseurl <> "/" <> uname <> "/" <> pname <> "/" <> ver <> ".tar.gz"
  let filename = T.unpack $ pname <> "-" <> ver <> ".tar.gz"
  let destDir = lp </> "packages"
  let destPath = destDir </> filename
  createDirectoryIfMissing True destDir
  resp <- get url
  -- writeFile destPath (resp ^. responseBody)

  undefined
