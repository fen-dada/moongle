{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Moongle.Registry where

import Control.Lens
import Control.Monad
import Data.ByteString.Lazy qualified as BL
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
import Effectful.Wreq
import Moongle.Config
import Moongle.General.Utils
import Moongle.Registry.Types
import Network.HTTP.Client (HttpException)
import System.FilePath
import Prelude hiding (writeFile)

-----------------------------------------------------------------------------

-- * Type‑level shorthands for common bundles of effects

-----------------------------------------------------------------------------

-- | All effects required by a plain HTTP call with logging.
type HttpEff es = (Wreq :> es, Reader Config :> es, Log :> es)

-- | Effects required to download & unpack a single package.
type FetchEff es =
  ( HttpEff es,
    FileSystem :> es,
    Error String :> es
  )

-----------------------------------------------------------------------------

-- * Domain types & helpers

-----------------------------------------------------------------------------

-- | A single package in the registry (owner/name/version).
--   Keeps domain logic out of string juggling.
data PackageId = PackageId
  { owner :: Text,
    name :: Text,
    version :: Text
  }
  deriving (Show, Eq)

-- | Convert a @ModuleInfo@ coming from the registry into a strongly‑typed
--   'PackageId'.
moduleInfoToPkg :: ModuleInfo -> Either String PackageId
moduleInfoToPkg ModuleInfo {..} =
  case T.splitOn "/" _name of
    (u : xs) | not (null xs) -> Right (PackageId u (mconcat xs) _version)
    _ -> Left $ "Malformed module name: " <> T.unpack _name

-----------------------------------------------------------------------------

-- * Pure helpers (no effects)

-----------------------------------------------------------------------------

-- | Construct the .zip download URL for a given package.
buildPackageUrl :: Config -> PackageId -> String
buildPackageUrl cfg PackageId {..} =
  T.unpack $ cfg ^. mooncakesBaseUrl <> "/" <> owner <> "/" <> name <> "/" <> version <> ".zip"

-- | Where on disk should we extract the package to?
packageDestinationDir :: Config -> PackageId -> FilePath
packageDestinationDir cfg PackageId {..} =
  let lp = T.unpack (cfg ^. moongleStoragePath)
      tag = T.unpack owner </> T.unpack name <> "-" <> T.unpack version
   in lp </> "packages" </> tag

-- | Destination path for the core runtime.
coreDestinationDir :: Config -> FilePath
coreDestinationDir cfg = T.unpack (cfg ^. moongleStoragePath) </> "packages" </> "moonbitlang"

-------------------------------------------------------------------------------

-- * High‑level actions – registry packages

-------------------------------------------------------------------------------

-- | Download and decode the package registry.
fetchRegistry :: (HttpEff es) => Eff es Registry
fetchRegistry = do
  url <- asks (T.unpack . (^. registryUrl))
  logInfo_ $ "Fetching registry from: " <> T.pack url
  resp <- get url
  reg <- (^. responseBody) <$> asJSON resp
  logInfo_ $
    "Successfully fetched registry with "
      <> T.pack (show (V.length (reg ^. modules)))
      <> " modules"
  pure reg

-- | Download *all* packages from the registry using a bounded thread‑pool.
fetchAllPackages :: (FetchEff es, Concurrent :> es) => Eff es ()
fetchAllPackages = do
  Registry mods <- fetchRegistry
  maxThreads <- asks (^. parallel)
  when (maxThreads <= 0) $ throwError_ @String "parallelism must be positive"

  successes <- pooledForConcurrentlyN maxThreads mods downloadOne
  logInfo_ $ "Downloaded " <> T.pack (show $ length successes) <> " packages successfully"
  where
    downloadOne mi =
      either (throwError_ @String) downloadPackage (moduleInfoToPkg mi)

-- | Download a *single* package and unzip it into the local storage
--   hierarchy.  All HTTP / IO exceptions are bubbled up via 'Error String'.
downloadPackage :: (FetchEff es) => PackageId -> Eff es ()
downloadPackage pkg@PackageId {..} = do
  cfg <- asks id
  let destDir = packageDestinationDir cfg pkg
  already <- doesDirectoryExist destDir

  if already
    then logInfo_ $ T.pack destDir <> " already exists, skipping download for " <> owner <> "-" <> name <> "-" <> version
    else do
      let url = buildPackageUrl cfg pkg
          tag = owner <> "-" <> name <> "-" <> version

      resp <- getWith opts url
      case resp ^. responseStatus . statusCode of
        200 -> do
          logInfo_ $ "Module downloaded successfully: " <> tag
          let zipBytes = resp ^. responseBody
          -- NOTE: unzip failure is considered a fatal error
          contents <- either (throwError_ . ("Failed to unzip module: " <>)) pure (unZip zipBytes)

          -- Write each file within the archive to its final location
          forM_ contents $ \(rawPath, bytes) -> do
            let rel = normalise rawPath
                out = destDir </> rel
            createDirectoryIfMissing True (takeDirectory out)
            writeFile out bytes
        s -> logAttention_ $ "Module failed to download: " <> tag <> " with status code: " <> T.pack (show s)

-----------------------------------------------------------------------------

-- * HTTP helpers

-----------------------------------------------------------------------------

-- | Wreq options that disable the default status‑code exception.
opts :: Options
opts = defaults & checkResponse ?~ (\_ _ -> pure ())
