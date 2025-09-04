{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Moongle.Registry where

import Control.Lens
import Control.Monad
import Data.Text (Text)
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
import Moongle.Config
import Moongle.Env
import Moongle.General.Utils
import Moongle.Registry.Types hiding (name, version)
import Servant.Client
import System.FilePath
import Prelude hiding (writeFile)

-----------------------------------------------------------------------------
-- Type‑level shorthands for common bundles of effects
-----------------------------------------------------------------------------

-- | All effects required by a plain HTTP call with logging.
type HttpEff es = (IOE :> es, Reader Config :> es, Log :> es, Reader Env :> es, Error String :> es)

-----------------------------------------------------------------------------
-- Domain types & helpers
-----------------------------------------------------------------------------

{- | A single package in the registry (owner/name/version).
  Keeps domain logic out of string juggling.
-}
data PackageId = PackageId
  { owner :: Text
  , name :: Text
  , version :: Text
  }
  deriving (Show, Eq)

{- | Convert a @ModuleInfo@ coming from the registry into a strongly‑typed
  'PackageId'.
-}
moduleInfoToPkg :: ModuleInfo -> Either String PackageId
moduleInfoToPkg ModuleInfo{..} =
  case T.splitOn "/" _name of
    (u : xs) | not (null xs) -> Right (PackageId u (mconcat xs) _version)
    _ -> Left $ "Malformed module name: " <> T.unpack _name

-----------------------------------------------------------------------------
-- Pure helpers (no effects)
-----------------------------------------------------------------------------

-- | Construct the .zip download URL for a given package.
buildPackageUrl :: Config -> PackageId -> String
buildPackageUrl cfg PackageId{..} =
  T.unpack $ cfg ^. mooncakesBaseUrl <> "/" <> owner <> "/" <> name <> "/" <> version <> ".zip"

-- | Where on disk should we extract the package to?
packageDestinationDir :: Config -> PackageId -> FilePath
packageDestinationDir cfg PackageId{..} =
  let lp = T.unpack (cfg ^. moongleStoragePath)
      tag = T.unpack owner </> T.unpack name <> "-" <> T.unpack version
   in lp </> "packages" </> tag

-- | Destination path for the core runtime.
coreDestinationDir :: Config -> FilePath
coreDestinationDir cfg = T.unpack (cfg ^. moongleStoragePath) </> "packages" </> "moonbitlang"

-------------------------------------------------------------------------------
-- High‑level actions – registry packages
-------------------------------------------------------------------------------

-- | Download and decode the package registry.
fetchRegistry :: (HttpEff es) => Eff es Registry
fetchRegistry = do
  url <- asks (T.unpack . (^. registryUrl))
  logInfo_ $ "Fetching registry from: " <> T.pack url

  Env{httpManager} <- ask
  Config{_registryUrl} <- ask
  let registryUrl = T.unpack _registryUrl
  resp <- liftIO $ runClientM queryRegistry (mkClientEnv httpManager (BaseUrl Https registryUrl 443 ""))
  reg <- either (throwError_ . show) pure resp

  logInfo_ $
    "Successfully fetched registry with "
      <> T.pack (show (V.length (reg ^. modules)))
      <> " modules"
  pure reg

-- | Download *all* packages from the registry using a bounded thread‑pool.
fetchAllPackages :: (HttpEff es, Concurrent :> es, FileSystem :> es) => Eff es ()
fetchAllPackages = do
  Registry mods <- fetchRegistry
  maxThreads <- asks (^. parallel)
  when (maxThreads <= 0) $ throwError_ @String "parallelism must be positive"

  results <- pooledForConcurrentlyN maxThreads (V.toList mods) $ \mi ->
    case moduleInfoToPkg mi of
      Left e -> do
        logAttention_ (T.pack e)
        pure Nothing
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
            logAttention_ $
              "[retry " <> T.pack (show k) <> "/" <> T.pack (show total) <> "] " <> T.pack err
            go (k + 1)

-- Download one package (with unzip + write to storage), throws on fatal errors inside.
downloadPackage :: (HttpEff es, FileSystem :> es) => PackageId -> Eff es ()
downloadPackage pkg@PackageId{..} = do
  cfg@Config{_mooncakesBaseUrl} <- asks id
  let mooncakesBaseUrl = T.unpack _mooncakesBaseUrl
  Env{httpManager} <- ask

  let destDir = packageDestinationDir cfg pkg
  already <- doesDirectoryExist destDir
  if already
    then logInfo_ $ T.pack destDir <> " already exists, skipping: " <> owner <> "-" <> name <> "-" <> version
    else do
      let verZip = version <> ".zip" -- API expects "0.1.0.zip"
          tag = owner <> "-" <> name <> "-" <> version
      logInfo_ $ "Downloading: " <> tag
      resp <- liftIO $ runClientM (queryMooncake owner name verZip) (mkClientEnv httpManager (BaseUrl Https mooncakesBaseUrl 443 ""))
      logInfo_ $ "Downloaded: " <> tag <> ", unzipping..."
      zipBytes <- either (throwError_ . show) pure resp
      contents <- either (throwError_ . ("Failed to unzip module: " <>)) pure (unZip zipBytes)
      forM_ contents $ \(rawPath, bytes) -> do
        let rel = normalise rawPath
            out = destDir </> rel
        createDirectoryIfMissing True (takeDirectory out)
        writeFile out bytes
      logInfo_ $ "Wrote " <> T.pack (show (length contents)) <> " files for " <> tag
