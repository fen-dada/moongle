{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Moongle.DB
  ( -- * Rows
    DefSummary (..),
    insertDefs,
    selectByTsQuery,
    selectByTsQueryInPkg,
    getStats,
    runMigrations
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only (Only), Query)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Effectful
import Effectful.PostgreSQL as EP
import Effectful.Log
import Moongle.DB.Types (DefRow (..))
import Moongle.TypeSearch

data DefSummary = DefSummary
  { username :: Text,
    mod :: Text,
    version :: Text,
    package :: [Text],
    funName :: Text,
    prettySig :: Text
  }
  deriving (Show, Eq)

instance FromRow DefSummary where
  fromRow = do
    u <- field
    m <- field
    v <- field
    PGArray pkg <- field -- pkg_path :: text[]
    fn <- field
    ps <- field
    pure
      DefSummary
        { username = u,
          mod = m,
          version = v,
          package = pkg,
          funName = fn,
          prettySig = ps
        }

insertDefs ::
  (WithConnection :> es, IOE :> es) =>
  [DefRow] ->
  Eff es Int64
insertDefs rows = do
  let sql =
        "INSERT INTO defs ( \
        \  username, mod, pkg_path, pkg_version, \
        \  fun_name, pretty_sig, visibility, kind, \
        \  tokens_lex, tokens, \
        \  arity, has_async, may_raise, \
        \  version_tag, src_file, src_line, src_col \
        \) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
  EP.executeMany sql (map defRowToParams rows)

defRowToParams :: DefRow -> [Action]
defRowToParams DefRow {..} =
  [ toField username,
    toField mod,
    toField (PGArray pkgPath),
    toField pkgVersion,
    toField funName,
    toField prettySig,
    toField visibility,
    toField kind,
    toField (PGArray tokensLex),
    toField (mkTSVector tokensLex), -- generate tsvector from tokens_lex
    toField arity,
    toField hasAsync,
    toField mayRaise,
    toField versionTag,
    toField srcFile,
    toField srcLine,
    toField srcCol
  ]

-- tsquery
selectByTsQuery :: (WithConnection :> es, IOE :> es) => Text -> Eff es [DefSummary]
selectByTsQuery tsq = do
  let sql :: Query
      sql =
        "SELECT \
        \  username, \"mod\", pkg_version, pkg_path, \
        \  fun_name, pretty_sig \
        \FROM defs \
        \WHERE tokens @@ (?::tsquery) \
        \ORDER BY username, mod, fun_name"
  EP.query sql (Only tsq)

-- (user, mod, version)
selectByTsQueryInPkg ::
  (WithConnection :> es, IOE :> es) =>
  Text -> -- tsquery
  (Text, Text, Text) -> -- (user, mod, version)
  Eff es [DefSummary]
selectByTsQueryInPkg tsq (u, m, v) = do
  let sql :: Query
      sql =
        "SELECT \
        \  username, mod, pkg_version, pkg_path, \
        \  fun_name, pretty_sig \
        \FROM defs \
        \WHERE tokens @@ to_tsquery('simple'::regconfig, ?) \
        \  AND username = ? AND mod = ? AND pkg_version = ? \
        \ORDER BY fun_name"
  EP.query sql (tsq, u, m, v)

-- Get stats
getStats :: (WithConnection :> es, IOE :> es) => Eff es (Int64, Int64)
getStats = do
  rows <-
    EP.query_
      "SELECT COUNT(*)::bigint AS defs_count, \
      \       COUNT(DISTINCT (username, pkg_path, pkg_version))::bigint AS pkg_count \
      \FROM defs"
  case rows of
    [(defsCnt, pkgCnt)] -> pure (defsCnt, pkgCnt)
    _ -> pure (0, 0)

runMigrations :: (EP.WithConnection :> es, IOE :> es, Log :> es) => Eff es ()
runMigrations = EP.withTransaction $ do
  logInfo_ "Running migrations…"

  _ <- EP.execute_ "DROP TABLE IF EXISTS defs"
  _ <-
    EP.execute_
      "CREATE TABLE IF NOT EXISTS defs ( \
      \ def_id bigserial PRIMARY KEY, \
      \ username text NOT NULL, \
      \ mod    text NOT NULL, \
      \ pkg_path text[] NOT NULL, \
      \ pkg_version text NOT NULL, \
      \ fun_name    text NOT NULL, \
      \ pretty_sig  text NOT NULL, \
      \ visibility  text NOT NULL, \
      \ kind        text NOT NULL, \
      \ tokens_lex  text[] NOT NULL, \
      \ tokens      tsvector NOT NULL, \
      \ arity int NOT NULL, \
      \ has_async boolean NOT NULL, \
      \ may_raise boolean NOT NULL, \
      \ version_tag text NOT NULL, \
      \ src_file text, src_line int, src_col int \
      \ )"

  -- gin index on tokens
  _ <- EP.execute_ "CREATE INDEX IF NOT EXISTS defs_tokens_gin ON defs USING gin (tokens)"
  _ <- EP.execute_ "CREATE INDEX IF NOT EXISTS defs_pkg ON defs (username, mod, pkg_version)"

  logInfo_ "Migrations done."
  pure ()
