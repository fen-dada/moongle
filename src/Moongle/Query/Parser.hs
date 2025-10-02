{-# LANGUAGE OverloadedStrings #-}

module Moongle.Query.Parser (pQuery, text2Query, pTyQuery, pNmQuery, text2TyQuery, text2NmQuery) where

import Control.Monad (when)
import Data.Char (isAlphaNum)
import Data.Functor
import Data.Text qualified as T
import Data.Text.Lazy (fromStrict)
import Language.Moonbit.Lexer
import Language.Moonbit.Mbti.Parser
import Language.Moonbit.Mbti.Syntax
import Moongle.Query.Syntax
import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

-- >>> parse pTyQuery "" "async [K : Compare, V](K, V) -> Int raise E"
-- Right (TyQuery (QueryModifiers {includePackages = [], excludePackages = []}) (TypeQuery {queryTyParams = [(TCon "K" [],[CTrait (TTrait Nothing "Compare")]),(TCon "V" [],[])], queryParams = [AnonParam False (TName Nothing (TCon "K" [])),AnonParam False (TName Nothing (TCon "V" []))], queryReturnTy = TName Nothing (TCon "Int" []), queryEff = [EffAsync,EffException (Araise (TName Nothing (TCon "E" [])))]}))

-- >>> parse pTyQuery "" "pkg:core (Int) -> String"
-- Right (TyQuery (QueryModifiers {includePackages = [["core"]], excludePackages = []}) (TypeQuery {queryTyParams = [], queryParams = [AnonParam False (TName Nothing (TCon "Int" []))], queryReturnTy = TName Nothing (TCon "String" []), queryEff = [EffException NoAraise]}))

pTyQuery :: Parser Query
pTyQuery = do
  modifiers <- pQueryModifiers
  TyQuery modifiers <$> pTyQueryCore

pTyQueryCore :: Parser TypeQuery
pTyQueryCore = do
  isAsync <- option False (reserved RWAsync $> True)
  typs <- option [] pTyParams
  params <- parens (commaSep pParam)
  reservedOp OpArrow
  retTy <- pType
  effEx <- option NoAraise pEffectException
  let effects = [EffAsync | isAsync] ++ [EffException effEx]
  pure $ TypeQuery typs params retTy effects

-- >>> parse pNmQuery "" "@btree/btree.insert"
-- Right (NmQuery (QueryModifiers {includePackages = [], excludePackages = []}) (NameQuery {queryModulePath = Just (TPath ["btree"] "btree"), queryName = TCon "insert" []}))

-- >>> parse pNmQuery "" "+pkg:core insert"
-- Right (NmQuery (QueryModifiers {includePackages = [["core"]], excludePackages = []}) (NameQuery {queryModulePath = Nothing, queryName = TCon "insert" []}))

pNmQuery :: Parser Query
pNmQuery = do
  modifiers <- pQueryModifiers
  NmQuery modifiers <$> pNmQueryCore

pNmQueryCore :: Parser NameQuery
pNmQueryCore = do
  mpath <- optionMaybe (pTPath <* reservedOp OpDot)
  NameQuery mpath <$> pTCon

pQuery :: Parser Query
pQuery = do
  modifiers <- pQueryModifiers
  try (TyQuery modifiers <$> pTyQueryCore)
    <|> (NmQuery modifiers <$> pNmQueryCore)

text2Query :: T.Text -> Either ParseError Query
text2Query = parse pQuery "" . fromStrict

text2TyQuery :: T.Text -> Either ParseError Query
text2TyQuery = parse pTyQuery "" . fromStrict

text2NmQuery :: T.Text -> Either ParseError Query
text2NmQuery = parse pNmQuery "" . fromStrict

data FilterKind = FilterInclude | FilterExclude

pQueryModifiers :: Parser QueryModifiers
pQueryModifiers = do
  whiteSpace
  filters <- many (try (pPackageFilter <* whiteSpace))
  let (incs, excs) = foldr step ([], []) filters
  pure $ QueryModifiers (reverse incs) (reverse excs)
  where
    step (FilterInclude, pathSegs) (includeAcc, excludeAcc) = (pathSegs : includeAcc, excludeAcc)
    step (FilterExclude, pathSegs) (includeAcc, excludeAcc) = (includeAcc, pathSegs : excludeAcc)

pPackageFilter :: Parser (FilterKind, [T.Text])
pPackageFilter = do
  sign <- optionMaybe (oneOf "+-")
  _ <- string "package:" <|> string "pkg:"
  raw <- many1 (satisfy isPkgChar)
  let segments = packageSegments (T.pack raw)
  when (null segments) $ fail "package selector cannot be empty"
  let kind = case sign of
        Just '-' -> FilterExclude
        _ -> FilterInclude
  pure (kind, segments)

isPkgChar :: Char -> Bool
isPkgChar c = isAlphaNum c || c `elem` ['_', '-', '/', '.']

packageSegments :: T.Text -> [T.Text]
packageSegments = filter (not . T.null) . fmap T.strip . T.splitOn "/"
