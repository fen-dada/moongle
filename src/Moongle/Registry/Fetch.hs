{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Moongle.Registry.Fetch
  ( HttpEff
  , fetchRegistry
  , fetchAllPackages
  , downloadPackage
  ) where

import Control.Lens ((^.))
import Control.Monad (forM_, when)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (pooledForConcurrentlyN)
import Effectful.Error.Static (Error, catchError, throwError)
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString.Lazy (writeFile)
import Effectful.Log (Log, logAttention_, logInfo_)
import Effectful.Reader.Static (Reader, ask, asks)
import Moongle.Config (Config, mooncakesBaseUrl, registryUrl)
import Moongle.Env (Env (..))
import Moongle.General.Utils (unZip)
import Moongle.Registry.Model
import Moongle.Registry.Types hiding (name, version)
import Servant.Client
import System.FilePath (normalise, takeDirectory, (</>))
import Prelude hiding (writeFile)

-- | All effects required by a plain HTTP call with logging.
type HttpEff es = (IOE :> es, Reader Config :> es, Log :> es, Reader Env :> es, Error String :> es)

fetchRegistry :: (HttpEff es) => Eff es Registry
fetchRegistry = do
  url <- asks (^. registryUrl)
  logInfo_ $ "Fetching registry from: " <> url
  Env{httpManager} <- ask
  let registryUrl = T.unpack url
  resp <- liftIO $ runClientM queryRegistry (mkClientEnv httpManager (BaseUrl Https registryUrl 443 ""))
  reg <- either (throwError . show) pure resp
  logInfo_ $
    "Successfully fetched registry with "
      <> T.pack (show (V.length (reg ^. modules)))
      <> " modules"
  pure reg

fetchAllPackages :: (HttpEff es, Concurrent :> es, FileSystem :> es) => Int -> Eff es ()
fetchAllPackages maxThreads = do
  when (maxThreads <= 0) $ throwError ("parallelism must be positive" :: String)
  Registry mods <- fetchRegistry
  results <- pooledForConcurrentlyN maxThreads (V.toList mods) $ \mi ->
    case moduleInfoToPkg mi of
      Left e -> logAttention_ (T.pack e) >> pure Nothing
      Right pkg -> do
        r <- retrying 3 (downloadPackage pkg)
        case r of
          Right () -> pure (Just ())
          Left err -> do
            let tag = owner pkg <> "-" <> name pkg <> "-" <> version pkg
            logAttention_ $ "Giving up after 3 tries: " <> tag <> " — " <> T.pack err
            pure Nothing
  let successes = length (filter (== Just ()) results)
  logInfo_ $ "Downloaded " <> T.pack (show successes) <> " / " <> T.pack (show (V.length mods)) <> " packages"

retrying :: (HttpEff es, FileSystem :> es) => Int -> Eff es a -> Eff es (Either String a)
retrying n0 act = go 1
 where
  total = max 1 n0
  go k = do
    e <- (Right <$> act) `catchError` (\_ e -> pure $ Left e)
    case e of
      Right a -> pure (Right a)
      Left err ->
        if k >= total
          then pure (Left err)
          else do
            logAttention_ $ "[retry " <> T.pack (show k) <> "/" <> T.pack (show total) <> "] " <> T.pack err
            go (k + 1)

-- Download one package (with unzip + write to storage), throws on fatal errors inside.
downloadPackage :: (HttpEff es, FileSystem :> es) => PackageId -> Eff es ()
downloadPackage pkg@PackageId{..} = do
  Env{httpManager} <- ask
  destDir <- asks (`packageDestinationDir` pkg)
  already <- doesDirectoryExist destDir
  if already
    then logInfo_ $ T.pack destDir <> " already exists, skipping: " <> owner <> "-" <> name <> "-" <> version
    else do
      let verZip = version <> ".zip"
          tag = owner <> "-" <> name <> "-" <> version
      baseUrl <- asks (^. mooncakesBaseUrl)
      let mooncakesBaseUrlTxt = T.unpack baseUrl
      logInfo_ $ "Downloading: " <> tag
      resp <- liftIO $ runClientM (queryMooncake owner name verZip) (mkClientEnv httpManager (BaseUrl Https mooncakesBaseUrlTxt 443 ""))
      logInfo_ $ "Downloaded: " <> tag <> ", unzipping..."
      zipBytes <- either (throwError . show) pure resp
      contents <- either (throwError . ("Failed to unzip module: " <>)) pure (unZip zipBytes)
      forM_ contents $ \(rawPath, bytes) -> do
        let rel = normalise rawPath
            out = destDir </> rel
        createDirectoryIfMissing True (takeDirectory out)
        writeFile out bytes
      logInfo_ $ "Wrote " <> T.pack (show (length contents)) <> " files for " <> tag
