module Moongle.Query.Loader
  ( collectMbtiFiles
  , collectMbtiFilesWithPkg
  ) where

import Control.Lens ((^.))
import Data.List (elemIndex)
import Data.Text qualified as T
import Effectful
import Effectful.FileSystem
import Effectful.Reader.Static (Reader, asks)
import Moongle.Config (Config, moongleStoragePath)
import Moongle.Registry.Model (PackageId (..))
import System.FilePath

collectMbtiFiles :: (FileSystem :> es, Reader Config :> es) => Eff es [FilePath]
collectMbtiFiles = do
  storageRoot <- asks (T.unpack . (^. moongleStoragePath))
  home <- getHomeDirectory
  let coreRoot = home </> ".moon" </> "lib" </> "core"
  coreMbti <- gatherMbtiFiles coreRoot
  customMbti <- gatherMbtiFiles storageRoot
  pure (coreMbti ++ customMbti)

collectMbtiFilesWithPkg :: (FileSystem :> es, Reader Config :> es) => Eff es [(PackageId, FilePath)]
collectMbtiFilesWithPkg = do
  storageRoot <- asks (T.unpack . (^. moongleStoragePath))
  let pkgRoot = storageRoot </> "packages"
  gather pkgRoot
 where
  gather :: (FileSystem :> es) => FilePath -> Eff es [(PackageId, FilePath)]
  gather path = do
    isDir <- doesDirectoryExist path
    if isDir
      then do
        entries <- listDirectory path
        concat <$> traverse gather [path </> e | e <- entries, e /= ".", e /= ".."]
      else pure $ maybe [] (\pid -> [(pid, path)]) (parsePkgIdFromPath path)

  parsePkgIdFromPath :: FilePath -> Maybe PackageId
  parsePkgIdFromPath fp =
    let dirs = splitDirectories (takeDirectory fp)
     in case elemIndex "packages" dirs of
          Just idx | idx + 2 < length dirs ->
            let ownerDir = dirs !! (idx + 1)
                nameVersion = dirs !! (idx + 2)
             in do
                  (pkgName, pkgVersion) <- splitNameVersion nameVersion
                  pure
                    PackageId
                      { owner = T.pack ownerDir
                      , name = T.pack pkgName
                      , version = T.pack pkgVersion
                      }
          _ -> Nothing

  splitNameVersion :: FilePath -> Maybe (String, String)
  splitNameVersion s =
    case break (== '-') (reverse s) of
      (revVer, '-' : revNameRev) -> do
        let ver = reverse revVer
            nm = reverse revNameRev
        if null nm || null ver then Nothing else Just (nm, ver)
      _ -> Nothing

-- Recursively collect all files ending in .mbti under a root.
gatherMbtiFiles :: (FileSystem :> es) => FilePath -> Eff es [FilePath]
gatherMbtiFiles root = do
  isDir <- doesDirectoryExist root
  if not isDir
    then pure [root | takeExtension root == ".mbti"]
    else do
      entries <- listDirectory root
      let children = [root </> e | e <- entries, e /= ".", e /= ".."]
      concat <$> traverse gatherMbtiFiles children
