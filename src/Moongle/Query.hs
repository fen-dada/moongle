{-# LANGUAGE OverloadedStrings #-}

module Moongle.Query where

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString.Lazy
import Effectful.Log
import Effectful.Reader.Static
import Language.Moonbit.Mbti
import Language.Moonbit.Mbti.Syntax
import Moongle.Config
import Moongle.Env
import Moongle.Query.Syntax
import System.FilePath
import Text.FuzzyFind
import Prelude hiding (readFile)
import Moongle.Registry (PackageId(..))

data QueryEntry = QueryEntry {qeModulePath :: ModulePath, qeDecl :: Decl}
  deriving (Show, Eq)

newtype QueryResult = QueryResult {qrDecls :: [(Alignment, QueryEntry)]}
  deriving (Show, Eq)

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
  case reverse (splitDirectories (takeDirectory fp)) of
    (nv:ownerDir:"packages":_) ->
      case breakRev '-' nv of
        Just (nm, ver) -> Just (PackageId (t ownerDir) (t nm) (t ver))
        _              -> Nothing
    _ -> Nothing
  where
    t = T.pack
    breakRev c s = case span (/= c) (reverse s) of
      (revVer, _ : revNmRev) -> Just (reverse revNmRev, reverse revVer)
      _                      -> Nothing

collectMbtiFilesWithPkg
  :: (FileSystem :> es, Reader Config :> es)
  => Eff es [(PackageId, FilePath)]
collectMbtiFilesWithPkg = do
  lp   <- asks (^. moongleStoragePath)
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
        else pure [ (pid, fp) | takeExtension fp == ".mbti", Just pid <- [parsePkgIdFromPath fp] ]

parseAllMbtiWithPkg
  :: (Reader Config :> es, FileSystem :> es, Log :> es, Error String :> es)
  => Eff es [(PackageId, MbtiFile)]
parseAllMbtiWithPkg = do
  paths <- collectMbtiFilesWithPkg
  xs <- forM paths $ \(pid, path) -> do
    b <- readFile path
    let contents = decodeUtf8 b
    case parseMbti path contents of
      Left err -> logAttention_ ("Failed to parse " <> T.pack path <> ": " <> T.pack (show err)) >> pure Nothing
      Right m  -> pure (Just (pid, m))
  let rs = catMaybes xs
  when (null rs) $ throwError @String "No MBTI files found or parsed"
  pure rs

query :: (Reader Env :> es, Log :> es) => Query -> Eff es QueryResult
query (NmQuery (NameQuery _ (TCon q _))) = do
  undefined
--   Env mbtis _ <- ask
--   -- NOTE: We only handle function declarations for now
--   let flattenedDecls = concatMap (\(MbtiFile mp _ decls) -> mapMaybe (mkEntry mp) decls) mbtis
--   let result = fuzzyFindOn getSearchString [q] flattenedDecls
--   pure $ QueryResult result
--   where
--     mkEntry mp d@FnDecl {} = Just (QueryEntry mp d)
--     mkEntry mp d@TypeDecl {} = Just (QueryEntry mp d)
--     mkEntry mp d@ConstDecl {} = Just (QueryEntry mp d)
--     mkEntry mp d@EnumDecl {} = Just (QueryEntry mp d)
--     mkEntry _ _ = Nothing
--
--     mkFunString :: FnDecl' -> String
--     mkFunString (FnDecl' (FnSig name _ _ _ _) _ _) = name
--
--     -- NOTE: SAFETY: We filtered the decls
--     getSearchString :: QueryEntry -> String
--     getSearchString (QueryEntry _ (FnDecl f)) = mkFunString f
--     getSearchString (QueryEntry _ (TypeDecl _ _ (TName _tpath (TCon name _)) _)) = name
--     getSearchString (QueryEntry _ (ConstDecl name _)) = name
--     getSearchString (QueryEntry _ (EnumDecl _ (TName _ (TCon name _)) _)) = name
--     getSearchString _ = error "Impossible"
-- query _ = do
--   logAttention_ "Type queries are not yet implemented"
--   pure $ QueryResult []
