{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Moongle.Registry.Model
  ( PackageId (..)
  , moduleInfoToPkg
  , buildPackageUrl
  , packageDestinationDir
  , coreDestinationDir
  ) where

import Control.Lens ((^.))
import Data.Text (Text)
import Data.Text qualified as T
import Moongle.Config (Config, moongleStoragePath, mooncakesBaseUrl)
import Moongle.Registry.Types (ModuleInfo (..))
import System.FilePath ((</>))

-- | A single package coordinate (owner/name/version).
data PackageId = PackageId
  { owner :: Text
  , name :: Text
  , version :: Text
  }
  deriving (Show, Eq)

moduleInfoToPkg :: ModuleInfo -> Either String PackageId
moduleInfoToPkg ModuleInfo{..} =
  case T.splitOn "/" _name of
    (u : xs) | not (null xs) -> Right (PackageId u (mconcat xs) _version)
    _ -> Left $ "Malformed module name: " <> T.unpack _name

buildPackageUrl :: Config -> PackageId -> String
buildPackageUrl cfg PackageId{..} =
  T.unpack $ cfg ^. mooncakesBaseUrl <> "/" <> owner <> "/" <> name <> "/" <> version <> ".zip"

packageDestinationDir :: Config -> PackageId -> FilePath
packageDestinationDir cfg PackageId{..} =
  let lp = T.unpack (cfg ^. moongleStoragePath)
      tag = T.unpack owner </> T.unpack name <> "-" <> T.unpack version
   in lp </> "packages" </> tag

coreDestinationDir :: Config -> FilePath
coreDestinationDir cfg = T.unpack (cfg ^. moongleStoragePath) </> "packages" </> "moonbitlang"
