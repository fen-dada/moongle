{-# LANGUAGE OverloadedStrings #-}

module Moongle.DB.Migrations
  ( runMigrations
  ) where

import Effectful
import Effectful.Log (Log, logInfo_)
import Effectful.PostgreSQL qualified as EP

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
  _ <- EP.execute_ "CREATE INDEX IF NOT EXISTS defs_tokens_gin ON defs USING gin (tokens)"
  _ <- EP.execute_ "CREATE INDEX IF NOT EXISTS defs_pkg ON defs (username, mod, pkg_version)"
  logInfo_ "Migrations done."
  pure ()
