{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Moongle.Registry where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString.Lazy
import Effectful.Reader.Static
import Effectful.Wreq
import GHC.Generics (Generic)
import Moongle.Config
import Moongle.General.Utils
import System.FilePath
import Prelude hiding (writeFile)

data ModuleInfo = ModuleInfo {_name :: T.Text, _version :: T.Text, _description :: T.Text}
  deriving (Show, Generic)

instance FromJSON ModuleInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

makeLenses ''ModuleInfo

newtype Registry = Registry {_modules :: V.Vector ModuleInfo}
  deriving (Show, Generic)

instance FromJSON Registry where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

makeLenses ''Registry

fetchRegistry :: (Wreq :> es, Reader Config :> es) => Eff es Registry
fetchRegistry = do
  regUrl <- T.unpack <$> asks (^. registryUrl)
  resp <- get regUrl
  reg <- asJSON resp
  pure $ reg ^. responseBody

downloadPackage :: (Wreq :> es, FileSystem :> es, Reader Config :> es, Error String :> es) => (T.Text, T.Text, T.Text) -> Eff es ()
downloadPackage (uname, pname, ver) = do
  lp <- T.unpack <$> asks (^. moongleStoragePath)
  baseurl <- asks (^. mooncakesBaseUrl)
  let url = T.unpack $ baseurl <> "/" <> uname <> "/" <> pname <> "/" <> ver <> ".zip"
  let destName = T.unpack $ pname <> "-" <> ver
  let destDir = lp </> "packages" </> destName
  createDirectoryIfMissing True destDir
  resp <- (^. responseBody) <$> get url
  contents <- case unZip resp of
    Left err -> throwError_ $ "Failed to unzip module: " <> err
    Right files -> pure files
  forM_ contents $ \(rawPath, bytes) -> do
    let rel = normalise rawPath
    let out = destDir </> rel
    createDirectoryIfMissing True (takeDirectory out)
    writeFile out bytes

fetchAll :: (Wreq :> es, FileSystem :> es, Reader Config :> es, Error String :> es, Concurrent :> es) => Eff es ()
fetchAll = do
  Registry mods <- fetchRegistry
  maxThreads <- asks (^. parallel)
  when (maxThreads <= 0) $ throwError_ @String "parallelism must be positive"
  pooledForConcurrentlyN_ maxThreads (V.toList mods) downloadOne
  where
    downloadOne ModuleInfo {..} =
      case T.splitOn "/" _name of
        [u, p] -> downloadPackage (u, p, _version)
        _ -> throwError_ $ "Malformed module name: " <> T.unpack _name
