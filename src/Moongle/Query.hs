{-# LANGUAGE OverloadedStrings #-}

module Moongle.Query where

import Data.List (elemIndex)
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Text qualified as T
import Data.Text (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString.Lazy
import Effectful.Log
import Effectful.PostgreSQL
import Effectful.Reader.Static
import Language.Moonbit.Mbti
import Language.Moonbit.Mbti.Syntax
import Moongle.Config
import Moongle.DB
import Moongle.DB.Index
import Moongle.Query.Syntax
import Moongle.Registry (PackageId (..))
import Moongle.TypeSearch
import System.FilePath
import Prelude hiding (readFile)
import Data.Int (Int64)

collectMbtiFiles :: (FileSystem :> es, Reader Config :> es) => Eff es [FilePath]
collectMbtiFiles = do
  lp <- asks (^. moongleStoragePath)
  home <- getHomeDirectory
  -- We use the core library bundled with moonbit toolchain here
  let corePath = home </> ".moon" </> "lib" </> "core"
  coreMbtiPaths <- go corePath
  mbtiPaths <- go $ T.unpack lp
  pure $ coreMbtiPaths ++ mbtiPaths
  where
    go :: (FileSystem :> es) => FilePath -> Eff es [FilePath]
    go fp = do
      isDir <- doesDirectoryExist fp
      if isDir
        then do
          entries <- listDirectory fp
          let subPaths = [fp </> e | e <- entries, e /= "." && e /= ".."]
          concat <$> traverse go subPaths
        else
          pure [fp | takeExtension fp == ".mbti"]

parseAllMbti :: (Reader Config :> es, FileSystem :> es, Log :> es, Error String :> es) => Eff es [MbtiFile]
parseAllMbti = do
  paths <- collectMbtiFiles
  mbtils <- forM paths $ \path -> do
    b <- readFile path
    let contents = decodeUtf8 b
    case parseMbti path contents of
      Left err -> do
        logAttention_ $ "Failed to parse " <> T.pack path <> ": " <> T.pack (show err)
        pure []
      Right mbti -> do
        logInfo_ $ "Successfully parsed " <> T.pack path
        pure [mbti]
  let mbtis = concat mbtils
  let num = length mbtis
  logInfo_ $
    "Parsed "
      <> T.pack (show $ length paths)
      <> " MBTI files, "
      <> T.pack (show num)
      <> " successful, "
      <> T.pack (show $ length paths - num)
      <> " failed"
  when (null mbtis) $ throwError @String "No MBTI files found or parsed"
  pure mbtis

parsePkgIdFromPath :: FilePath -> Maybe PackageId
parsePkgIdFromPath fp =
  let dirs = splitDirectories (takeDirectory fp)
  in case elemIndex "packages" dirs of
       Just i | i + 2 < length dirs ->
         let ownerDir    = dirs !! (i + 1)
             nameVersion = dirs !! (i + 2)      -- e.g. "elk-0.1.0"
         in case splitNameVersion nameVersion of
              Just (nm, ver) -> Just (PackageId (t ownerDir) (t nm) (t ver))
              Nothing        -> Nothing
       _ -> Nothing
  where
    t = T.pack
    splitNameVersion s =
      case break (=='-') (reverse s) of
        (revVer, '-' : revNameRev) ->
          let ver = reverse revVer
              nm  = reverse revNameRev
          in if null nm || null ver then Nothing else Just (nm, ver)
        _ -> Nothing

collectMbtiFilesWithPkg ::
  (FileSystem :> es, Reader Config :> es) =>
  Eff es [(PackageId, FilePath)]
collectMbtiFilesWithPkg = do
  lp <- asks (^. moongleStoragePath)
  let root = T.unpack lp </> "packages"
  go root
  where
    go :: (FileSystem :> es) => FilePath -> Eff es [(PackageId, FilePath)]
    go fp = do
      isDir <- doesDirectoryExist fp
      if isDir
        then do
          entries <- listDirectory fp
          concat <$> traverse go [fp </> e | e <- entries, e /= ".", e /= ".."]
        else pure [(pid, fp) | takeExtension fp == ".mbti", Just pid <- [parsePkgIdFromPath fp]]

parseAllMbtiWithPkg ::
  (Reader Config :> es, FileSystem :> es, Log :> es, Error String :> es) =>
  Eff es [(PackageId, MbtiFile)]
parseAllMbtiWithPkg = do
  paths <- collectMbtiFilesWithPkg
  xs <- forM paths $ \(pid, path) -> do
    b <- readFile path
    let contents = decodeUtf8 b
    case parseMbti path contents of
      Left err -> logAttention_ ("Failed to parse " <> T.pack path <> ": " <> T.pack (show err)) >> pure Nothing
      Right m -> pure (Just (pid, m))
  let rs = catMaybes xs
  when (null rs) $ throwError @String "No MBTI files found or parsed"
  pure rs

insertMbtiToDB :: (WithConnection :> es, Log :> es, IOE :> es) => [(PackageId, MbtiFile)] -> Eff es Int64
insertMbtiToDB xs = do
  logInfo_ $ "insertMbtiToDB: " <> T.pack (show $ length xs) <> " MBTI files"
  let rows = concatMap (uncurry mbtiToDefRows) xs
  logInfo_ $ "Inserting " <> T.pack (show $ length rows) <> " definitions into database"
  insertDefs rows <* logInfo_ "Insert completed"

escapeTsqueryLexeme :: Text -> Text
escapeTsqueryLexeme = T.replace "'" "''"

buildTsQueryStrict :: [Text] -> Text
buildTsQueryStrict = buildTsQueryStrictWith (const False)

buildTsQuery :: [Text] -> Text
buildTsQuery = buildTsQueryStrictWith (T.isSuffixOf ".")

buildTsQueryStrictWith :: (Text -> Bool) -> [Text] -> Text
buildTsQueryStrictWith shouldPrefix toks =
  T.intercalate " & " (map one toks)
  where
    one t =
      let lit = "'" <> escapeTsqueryLexeme t <> "'"
      in if shouldPrefix t then lit <> ":*" else lit

queryDefSummary :: (WithConnection :> es, Log :> es, IOE :> es) => Query -> Eff es [DefSummary]
queryDefSummary q = do
  let qlxm = lexemesForQuery q
  let tsq = buildTsQuery qlxm
  logInfo_ $ "Querying with tsquery: " <> tsq
  selectByTsQuery tsq

-- query :: (WithConnection :> es, Log :> es, IOE :> es) => Query -> Eff es QueryResult
-- query q = do
--   logInfo_ $ "Querying: " <> T.pack (show q)
--   defsums <- queryDefSummary q
--
--
