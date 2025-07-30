{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Moongle.Registry where

import Control.Exception qualified as E
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString.Lazy
import Effectful.Log
import Effectful.Reader.Static
import Effectful.Wreq
import GHC.Generics (Generic)
import Moongle.Config
import Moongle.General.Utils
import Network.HTTP.Client (HttpException)
import System.FilePath
import Prelude hiding (writeFile)

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

fetchRegistry :: (Wreq :> es, Reader Config :> es, Log :> es) => Eff es Registry
fetchRegistry = do
  regtext <- asks (^. registryUrl)
  let regUrl = T.unpack regtext
  logInfo_ $ "Fetching registry from: " <> regtext
  resp <- get regUrl
  reg <- (^. responseBody) <$> asJSON resp
  logInfo_ $ "Successfully fetched registry with " <> T.pack (show (V.length $ reg ^. modules)) <> " modules"
  pure reg

downloadPackage :: (Wreq :> es, FileSystem :> es, Reader Config :> es, Error String :> es, Log :> es) => (T.Text, T.Text, T.Text) -> Eff es ()
downloadPackage (uname, pname, ver) = do
  lp <- T.unpack <$> asks (^. moongleStoragePath)
  baseurl <- asks (^. mooncakesBaseUrl)
  let tag = uname <> "-" <> pname <> "-" <> ver
  let url = T.unpack $ baseurl <> "/" <> uname <> "/" <> pname <> "/" <> ver <> ".zip"
  let destName = T.unpack $ pname <> "-" <> ver
  let destDir = lp </> "packages" </> T.unpack uname </> destName

  already <- doesDirectoryExist destDir
  if already
    then do
      logInfo_ $ T.pack destDir <> " already exists, skipping download for " <> tag
    else do
      respE <- getWith opts url
      case respE ^. responseStatus . statusCode of
        200 -> do
          logInfo_ $ "Module downloaded successfully: " <> uname <> "-" <> pname <> "-" <> ver
          let resp = (^. responseBody) respE

          contents <- either (throwError_ . ("Failed to unzip module" <>)) pure (unZip resp)

          forM_ contents $ \(rawPath, bytes) -> do
            let rel = normalise rawPath
            let out = destDir </> rel
            createDirectoryIfMissing True (takeDirectory out)
            writeFile out bytes
        s -> logInfo_ $ "Module failed to download: " <> uname <> "-" <> pname <> "-" <> ver <> "with status code: " <> T.pack (show s)

fetchAll :: (Error String :> es, Reader Config :> es, Wreq :> es, FileSystem :> es, Concurrent :> es, Log :> es) => Eff es ()
fetchAll = do
  Registry mods <- fetchRegistry
  maxThreads <- asks (^. parallel)
  when (maxThreads <= 0) $ throwError_ @String "parallelism must be positive"
  pooledForConcurrentlyN_ maxThreads mods downloadOne
  where
    downloadOne ModuleInfo {..} =
      case T.splitOn "/" _name of
        u : xs -> downloadPackage (u, mconcat xs, _version)
        _ -> throwError_ $ "Malformed module name: " <> T.unpack _name

safeGet :: (IOE :> es, Wreq :> es) => String -> Eff es (Either HttpException (Response BL.ByteString))
safeGet url = withRunInIO (\run -> E.try @HttpException (run (get url)))

opts :: Effectful.Wreq.Options
opts = defaults & checkResponse ?~ (\_ _ -> pure ())
