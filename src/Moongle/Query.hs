{-# LANGUAGE OverloadedStrings #-}

module Moongle.Query where

import Control.Lens
import Control.Monad
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

data QueryEntry = QueryEntry {qeModulePath :: ModulePath, qeDecl :: Decl}
  deriving (Show, Eq)

newtype QueryResult = QueryResult {qrDecls :: [(Alignment, QueryEntry)]}
  deriving (Show, Eq)

collectMbtiFiles :: (FileSystem :> es, Reader Config :> es) => Eff es [FilePath]
collectMbtiFiles = do
  lp <- asks (^. moongleStoragePath)
  go $ T.unpack lp
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
  logInfo_ $ "Parsed " <> T.pack (show num) <> " MBTI files, " <> T.pack (show num) <> " successful"
  when (null mbtis) $ throwError @String "No MBTI files found or parsed"
  pure mbtis

query :: (Reader Env :> es, Log :> es) => Query -> Eff es QueryResult
query (NmQuery (NameQuery _ (TCon q _))) = do
  Env mbtis _ <- ask
  -- NOTE: We only handle function declarations for now
  let flattenedDecls = concatMap (\(MbtiFile mp _ decls) -> [QueryEntry mp d | d@FnDecl {} <- decls]) mbtis
  let result = fuzzyFindOn getFunString [q] flattenedDecls
  pure $ QueryResult result
  where
    makeFunString :: FnDecl' -> String
    makeFunString (FnDecl' (FnSig name _ _ _ _) _ _) = name

    -- NOTE: SAFETY: We filtered the decls
    getFunString :: QueryEntry -> String
    getFunString (QueryEntry _ (FnDecl f)) = makeFunString f
    getFunString _ = error "Impossible"
query _ = do
  logAttention_ "Type queries are not yet implemented"
  pure $ QueryResult []
