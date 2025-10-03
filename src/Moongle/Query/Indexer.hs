{-# LANGUAGE OverloadedStrings #-}

module Moongle.Query.Indexer
  ( parseAllMbti
  , parseAllMbtiWithPkg
  , upsertPackages
  ) where

import Control.Monad (when)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString.Lazy (readFile)
import Effectful.Log
import Effectful.PostgreSQL (WithConnection)
import Effectful.Reader.Static (Reader)
import Language.Moonbit.Mbti (parseMbti)
import Language.Moonbit.Mbti.Syntax (MbtiFile)
import Moongle.Config (Config)
import Moongle.DB.Index (mbtiToDefRows)
import Moongle.DB.Insert (insertDefs)
import Moongle.Query.Loader (collectMbtiFiles, collectMbtiFilesWithPkg)
import Moongle.Registry.Model (PackageId)
import Prelude hiding (readFile)

parseAllMbti :: (Reader Config :> es, FileSystem :> es, Log :> es, Error String :> es) => Eff es [MbtiFile]
parseAllMbti = do
  paths <- collectMbtiFiles
  parsed <- traverse parse paths
  let mbtis = concat parsed
  summarise paths mbtis
  when (null mbtis) $ throwError ("No MBTI files found or parsed" :: String)
  pure mbtis
 where
  parse path = do
    bytes <- readFile path
    let contents = decodeUtf8 bytes
    case parseMbti path contents of
      Left err -> logAttention_ ("Failed to parse " <> T.pack path <> ": " <> T.pack (show err)) >> pure []
      Right mbti -> logInfo_ ("Successfully parsed " <> T.pack path) >> pure [mbti]

parseAllMbtiWithPkg :: (Reader Config :> es, FileSystem :> es, Log :> es, Error String :> es) => Eff es [(PackageId, MbtiFile)]
parseAllMbtiWithPkg = do
  paths <- collectMbtiFilesWithPkg
  parsed <- traverse parse paths
  let results = catMaybes parsed
  summarise (map snd paths) (map snd results)
  when (null results) $ throwError ("No MBTI files found or parsed" :: String)
  pure results
 where
  parse (pid, path) = do
    bytes <- readFile path
    let contents = decodeUtf8 bytes
    case parseMbti path contents of
      Left err -> logAttention_ ("Failed to parse " <> T.pack path <> ": " <> T.pack (show err)) >> pure Nothing
      Right mbti -> logInfo_ ("Successfully parsed " <> T.pack path) >> pure (Just (pid, mbti))

upsertPackages :: (WithConnection :> es, Log :> es, IOE :> es) => [(PackageId, MbtiFile)] -> Eff es Int64
upsertPackages packages = do
  logInfo_ $ "Indexing " <> T.pack (show (length packages)) <> " MBTI files"
  let rows = concatMap (uncurry mbtiToDefRows) packages
  logInfo_ $ "Inserting " <> T.pack (show (length rows)) <> " definitions into database"
  inserted <- insertDefs rows
  logInfo_ "Insert completed"
  pure inserted

summarise :: (Log :> es) => [FilePath] -> [MbtiFile] -> Eff es ()
summarise paths successes =
  logInfo_ $
    "Parsed "
      <> T.pack (show (length paths))
      <> " MBTI files, "
      <> T.pack (show (length successes))
      <> " successful, "
      <> T.pack (show (length paths - length successes))
      <> " failed"
