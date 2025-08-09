{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Moongle.DB
  ( -- * Rows
    DefSummary (..),
    insertDefs,
    selectByTsQuery,
    selectByTsQueryInPkg,
    getStats,
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Database.PostgreSQL.Simple (Only (Only), Query)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Effectful
import Effectful.PostgreSQL as EP
import Moongle.DB.Types (DefRow (..))

--------------------------------------------------------------------------------
-- 查询结果精简行
--------------------------------------------------------------------------------

data DefSummary = DefSummary
  { pkgOwner :: Text,
    pkgName :: Text,
    pkgVersion :: Text,
    modUser :: Text,
    modName :: Text,
    funName :: Text,
    prettySig :: Text
  }
  deriving (Show, Eq)

instance FromRow DefSummary where
  fromRow = DefSummary <$> field <*> field <*> field <*> field <*> field <*> field <*> field

--------------------------------------------------------------------------------
-- 批量插入
--------------------------------------------------------------------------------

-- | 批量插入 defs（只写 tokens_lex，其余由数据库生成列/触发器维护）。
insertDefs ::
  (EP.WithConnection :> es, IOE :> es) =>
  [DefRow] ->
  Eff es Int64
insertDefs rows = do
  -- 注意：postgresql-simple 用 '?' 占位符
  let sql :: Database.PostgreSQL.Simple.Query
      sql =
        "INSERT INTO defs ( \
        \ pkg_owner, pkg_name, pkg_version, \
        \ mod_user, mod_name, mod_pkg_path, \
        \ fun_name, pretty_sig, visibility, kind, \
        \ tokens_lex, arity, has_async, may_raise, \
        \ version_tag, src_file, src_line, src_col \
        \) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
  EP.executeMany sql (map defRowToParams rows)

-- 将 DefRow 序列化为一行参数
defRowToParams :: DefRow -> [Action]
defRowToParams DefRow {..} =
  [ toField pkgOwner,
    toField pkgName,
    toField pkgVersion,
    toField modUser,
    toField modName,
    toField (PGArray modPkgPath), -- text[]
    toField funName,
    toField prettySig,
    toField visibility,
    toField kind,
    toField (PGArray tokensLex), -- text[]
    toField arity,
    toField hasAsync,
    toField mayRaise,
    toField versionTag,
    toField srcFile,
    toField srcLine,
    toField srcCol
  ]

--------------------------------------------------------------------------------
-- 查询（@@ to_tsquery('simple', ?)）
--------------------------------------------------------------------------------

-- | 用 tsquery 文本查询（例如：'''attr,async.'' & ''mn,r,String.''）
selectByTsQuery ::
  (EP.WithConnection :> es, IOE :> es) =>
  -- | tsquery 文本（不加引号，原样传）
  Text ->
  Eff es [DefSummary]
selectByTsQuery tsq = do
  let sql :: Database.PostgreSQL.Simple.Query
      sql =
        "SELECT \
        \  pkg_owner, pkg_name, pkg_version, \
        \  mod_user, mod_name, fun_name, pretty_sig \
        \FROM defs \
        \WHERE tokens @@ to_tsquery('simple', ?) \
        \ORDER BY pkg_owner, pkg_name, fun_name \
        \LIMIT 200"
  EP.query sql (Database.PostgreSQL.Simple.Only (TE.encodeUtf8 tsq))

-- | 限定包（owner/name/version）内做 tsquery。
selectByTsQueryInPkg ::
  (EP.WithConnection :> es, IOE :> es) =>
  -- | tsquery 文本
  Text ->
  -- | (owner, name, version)
  (Text, Text, Text) ->
  Eff es [DefSummary]
selectByTsQueryInPkg tsq (own, nm, ver) = do
  let sql :: Database.PostgreSQL.Simple.Query
      sql =
        "SELECT \
        \  pkg_owner, pkg_name, pkg_version, \
        \  mod_user, mod_name, fun_name, pretty_sig \
        \FROM defs \
        \WHERE tokens @@ to_tsquery('simple', ?) \
        \  AND pkg_owner = ? AND pkg_name = ? AND pkg_version = ? \
        \ORDER BY fun_name \
        \LIMIT 200"
  EP.query sql (TE.encodeUtf8 tsq, own, nm, ver)

getStats :: (WithConnection :> es, IOE :> es) => Eff es (Int64, Int64)
getStats = do
  rows <- EP.query_
    "SELECT COUNT(*)::bigint AS defs_count, \
    \       COUNT(DISTINCT (pkg_owner, pkg_name, pkg_version))::bigint AS pkg_count \
    \FROM defs"
  let (defs, pkgs) = case rows of
        [(defsCnt, pkgsCnt)] -> (defsCnt, pkgsCnt)
        _                    -> (0, 0)
  pure (defs, pkgs)
