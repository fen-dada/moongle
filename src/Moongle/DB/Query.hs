{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Moongle.DB.Query
  ( selectByTsQuery
  , selectByTsQueryInPkg
  , getStats
  , renderPackageFilters
  ) where

import Data.Int (Int64)
import Data.List (intercalate)
import Data.String (fromString)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.ToField (Action, ToField (toField))
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Effectful
import Effectful.PostgreSQL as EP
import Moongle.DB.Definition (DefSummary)
import Moongle.Query.Syntax (QueryModifiers (..))

newtype DynamicParams = DynamicParams [Action]

instance ToRow DynamicParams where
  toRow (DynamicParams xs) = xs

selectByTsQuery :: (WithConnection :> es, IOE :> es) => Text -> QueryModifiers -> Eff es [DefSummary]
selectByTsQuery tsq modifiers = do
  let (filterSql, filterParams) = renderPackageFilters modifiers
      sqlText =
        "SELECT \
        \  username, \"mod\", pkg_version, pkg_path, \
        \  fun_name, pretty_sig \
        \FROM defs \
        \WHERE tokens @@ (?::tsquery)"
          <> filterSql
          <> " \
        \ORDER BY username, mod, fun_name"
      params = DynamicParams (toField tsq : filterParams)
  EP.query (fromString sqlText) params

selectByTsQueryInPkg :: (WithConnection :> es, IOE :> es) => Text -> (Text, Text, Text) -> Eff es [DefSummary]
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

renderPackageFilters :: QueryModifiers -> (String, [Action])
renderPackageFilters QueryModifiers{includePackages, excludePackages} =
  let (includeSql, includeParams) = clauses " AND (" " OR " ")" includePackages
      (excludeSql, excludeParams) = clauses " AND NOT (" " OR " ")" excludePackages
   in (includeSql <> excludeSql, includeParams <> excludeParams)
 where
  clauses :: String -> String -> String -> [[Text]] -> (String, [Action])
  clauses prefix joiner suffix rawSegments =
    let useful = filter (not . null) rawSegments
     in if null useful
          then ("", [])
          else
            let placeholders = replicate (length useful) "pkg_path = ?"
                sqlFragment = prefix <> intercalate joiner placeholders <> suffix
                params = map (toField . PGArray) useful
             in (sqlFragment, params)
