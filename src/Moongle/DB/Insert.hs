{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Moongle.DB.Insert
  ( insertDefs
  , defRowToParams
  ) where

import Data.Int (Int64)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.ToField (Action, toField)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Effectful
import Effectful.PostgreSQL as EP
import Moongle.DB.Types (DefRow (..))
import Moongle.TypeSearch (mkTSVector)

insertDefs :: (WithConnection :> es, IOE :> es) => [DefRow] -> Eff es Int64
insertDefs rows = do
  let sql :: Query
      sql =
        "INSERT INTO defs ( \
        \  username, mod, pkg_path, pkg_version, \
        \  fun_name, pretty_sig, visibility, kind, \
        \  tokens_lex, tokens, \
        \  arity, has_async, may_raise, \
        \  version_tag, src_file, src_line, src_col \
        \) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
  EP.executeMany sql (map defRowToParams rows)

defRowToParams :: DefRow -> [Action]
defRowToParams DefRow{username, mod = moduleName, pkgPath, pkgVersion, funName, prettySig, visibility, kind, tokensLex, arity, hasAsync, mayRaise, versionTag, srcFile, srcLine, srcCol} =
  [ toField username
  , toField moduleName
  , toField (PGArray pkgPath)
  , toField pkgVersion
  , toField funName
  , toField prettySig
  , toField visibility
  , toField kind
  , toField (PGArray tokensLex)
  , toField (mkTSVector tokensLex)
  , toField arity
  , toField hasAsync
  , toField mayRaise
  , toField versionTag
  , toField srcFile
  , toField srcLine
  , toField srcCol
  ]
